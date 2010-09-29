module Haml
  # This class is used only internally. It holds the buffer of HTML that
  # is eventually output as the resulting document.
  # It's called from within the precompiled code,
  # and helps reduce the amount of processing done within `instance_eval`ed code.
  class Buffer
    include Haml::Helpers
    include Haml::Util

    # The string that holds the compiled HTML. This is aliased as
    # `_erbout` for compatibility with ERB-specific code.
    #
    # @return [String]
    attr_accessor :buffer

    # The options hash passed in from {Haml::Engine}.
    #
    # @return [{String => Object}]
    # @see Haml::Engine#options_for_buffer
    attr_accessor :options

    # The {Buffer} for the enclosing Haml document.
    # This is set for partials and similar sorts of nested templates.
    # It's `nil` at the top level (see \{#toplevel?}).
    #
    # @return [Buffer]
    attr_accessor :upper

    # nil if there's no capture_haml block running,
    # and the position at which it's beginning the capture if there is one.
    #
    # @return [Fixnum, nil]
    attr_accessor :capture_position

    # @return [Boolean]
    # @see #active?
    attr_writer :active

    # @return [Boolean] Whether or not the format is XHTML
    def xhtml?
      not html?
    end

    # @return [Boolean] Whether or not the format is any flavor of HTML
    def html?
      html4? or html5?
    end

    # @return [Boolean] Whether or not the format is HTML4
    def html4?
      @options[:format] == :html4
    end

    # @return [Boolean] Whether or not the format is HTML5.
    def html5?
      @options[:format] == :html5
    end

    # @return [Boolean] Whether or not this buffer is a top-level template,
    #   as opposed to a nested partial
    def toplevel?
      upper.nil?
    end

    # Whether or not this buffer is currently being used to render a Haml template.
    # Returns `false` if a subtemplate is being rendered,
    # even if it's a subtemplate of this buffer's template.
    #
    # @return [Boolean]
    def active?
      @active
    end

    # @return [Fixnum] The current indentation level of the document
    def tabulation
      @real_tabs + @tabulation
    end

    # Sets the current tabulation of the document.
    #
    # @param val [Fixnum] The new tabulation
    def tabulation=(val)
      val = val - @real_tabs
      @tabulation = val > -1 ? val : 0
    end

    # @param upper [Buffer] The parent buffer
    # @param options [{Symbol => Object}] An options hash.
    #   See {Haml::Engine#options\_for\_buffer}
    def initialize(upper = nil, options = {})
      @active = true
      @upper = upper
      @options = options
      @buffer = ruby1_8? ? "" : "".encode(Encoding.find(options[:encoding]))
      @tabulation = 0

      # The number of tabs that Engine thinks we should have
      # @real_tabs + @tabulation is the number of tabs actually output
      @real_tabs = 0
    end

    # Appends text to the buffer, properly tabulated.
    # Also modifies the document's indentation.
    #
    # @param text [String] The text to append
    # @param tab_change [Fixnum] The number of tabs by which to increase
    #   or decrease the document's indentation
    # @param dont_tab_up [Boolean] If true, don't indent the first line of `text`
    def push_text(text, tab_change, dont_tab_up)
      if @tabulation > 0
        # Have to push every line in by the extra user set tabulation.
        # Don't push lines with just whitespace, though,
        # because that screws up precompiled indentation.
        text.gsub!(/^(?!\s+$)/m, tabs)
        text.sub!(tabs, '') if dont_tab_up
      end

      @buffer << text
      @real_tabs += tab_change
    end

    # Modifies the indentation of the document.
    #
    # @param tab_change [Fixnum] The number of tabs by which to increase
    #   or decrease the document's indentation
    def adjust_tabs(tab_change)
      @real_tabs += tab_change
    end

    Haml::Util.def_static_method(self, :format_script, [:result],
                                 :preserve_script, :in_tag, :preserve_tag, :escape_html,
                                 :nuke_inner_whitespace, :interpolated, :ugly, <<RUBY)
      <% # Escape HTML here so that the safety of the string is preserved in Rails
         result_name = escape_html ? "html_escape(result.to_s)" : "result.to_s" %>
      <% unless ugly %>
        # If we're interpolated,
        # then the custom tabulation is handled in #push_text.
        # The easiest way to avoid it here is to reset @tabulation.
        <% if interpolated %>
          old_tabulation = @tabulation
          @tabulation = 0
        <% end %>

        tabulation = @real_tabs
        result = <%= result_name %>.<% if nuke_inner_whitespace %>strip<% else %>rstrip<% end %>
      <% else %>
        result = <%= result_name %><% if nuke_inner_whitespace %>.strip<% end %>
      <% end %>

      <% if preserve_tag %>
        result = Haml::Helpers.preserve(result)
      <% elsif preserve_script %>
        result = Haml::Helpers.find_and_preserve(result, options[:preserve])
      <% end %>

      <% if ugly %>
        return result
      <% else %>

        has_newline = result.include?("\\n") 
        <% if in_tag && !nuke_inner_whitespace %>
          <% unless preserve_tag %> if !has_newline <% end %>
          @real_tabs -= 1
          <% if interpolated %> @tabulation = old_tabulation <% end %>
          return result
          <% unless preserve_tag %> end <% end %>
        <% end %>

        # Precompiled tabulation may be wrong
        <% if !interpolated && !in_tag %>
          result = tabs + result if @tabulation > 0
        <% end %>

        if has_newline
          result = result.gsub "\\n", "\\n" + tabs(tabulation)

          # Add tabulation if it wasn't precompiled
          <% if in_tag && !nuke_inner_whitespace %> result = tabs(tabulation) + result <% end %>
        end

        <% if in_tag && !nuke_inner_whitespace %>
          result = "\\n\#{result}\\n\#{tabs(tabulation-1)}"
          @real_tabs -= 1
        <% end %>
        <% if interpolated %> @tabulation = old_tabulation <% end %>
        result
      <% end %>
RUBY

    # Takes the various information about the opening tag for an element,
    # formats it, and appends it to the buffer.
    def open_tag(name, self_closing, try_one_line, preserve_tag, escape_html, class_id,
                 nuke_outer_whitespace, nuke_inner_whitespace, obj_ref, content, *attributes_hashes)
      tabulation = @real_tabs

      attributes = class_id
      attributes_hashes.each do |old|
        self.class.merge_attrs(attributes, to_hash(old.map {|k, v| [k.to_s, v]}))
      end
      self.class.merge_attrs(attributes, parse_object_ref(obj_ref)) if obj_ref

      if self_closing && xhtml?
        str = " />" + (nuke_outer_whitespace ? "" : "\n")
      else
        str = ">" + ((if self_closing && html?
                        nuke_outer_whitespace
                      else
                        try_one_line || preserve_tag || nuke_inner_whitespace
                      end) ? "" : "\n")
      end

      attributes = Precompiler.build_attributes(html?, @options[:attr_wrapper], attributes)
      @buffer << "#{nuke_outer_whitespace || @options[:ugly] ? '' : tabs(tabulation)}<#{name}#{attributes}#{str}"

      if content
        @buffer << "#{content}</#{name}>" << (nuke_outer_whitespace ? "" : "\n")
        return
      end

      @real_tabs += 1 unless self_closing || nuke_inner_whitespace
    end

    # Remove the whitespace from the right side of the buffer string.
    # Doesn't do anything if we're at the beginning of a capture_haml block.
    def rstrip!
      if capture_position.nil?
        buffer.rstrip!
        return
      end

      buffer << buffer.slice!(capture_position..-1).rstrip
    end

    # Merges two attribute hashes.
    # This is the same as `to.merge!(from)`,
    # except that it merges id, class, and data attributes.
    #
    # ids are concatenated with `"_"`,
    # and classes are concatenated with `" "`.
    # data hashes are simply merged.
    #
    # Destructively modifies both `to` and `from`.
    #
    # @param to [{String => String}] The attribute hash to merge into
    # @param from [{String => #to_s}] The attribute hash to merge from
    # @return [{String => String}] `to`, after being merged
    def self.merge_attrs(to, from)
      from['id'] = Precompiler.filter_and_join(from['id'], '_') if from['id']
      if to['id'] && from['id']
        to['id'] << '_' << from.delete('id').to_s
      elsif to['id'] || from['id']
        from['id'] ||= to['id']
      end

      from['class'] = Precompiler.filter_and_join(from['class'], ' ') if from['class']
      if to['class'] && from['class']
        # Make sure we don't duplicate class names
        from['class'] = (from['class'].to_s.split(' ') | to['class'].split(' ')).sort.join(' ')
      elsif to['class'] || from['class']
        from['class'] ||= to['class']
      end

      from_data = from['data'].is_a?(Hash)
      to_data = to['data'].is_a?(Hash)
      if from_data && to_data
        to['data'] = to['data'].merge(from['data'])
      elsif to_data
        to = Haml::Util.map_keys(to.delete('data')) {|name| "data-#{name}"}.merge(to)
      elsif from_data
        from = Haml::Util.map_keys(from.delete('data')) {|name| "data-#{name}"}.merge(from)
      end

      to.merge!(from)
    end

    private

    @@tab_cache = {}
    # Gets `count` tabs. Mostly for internal use.
    def tabs(count = 0)
      tabs = [count + @tabulation, 0].max
      @@tab_cache[tabs] ||= '  ' * tabs
    end

    # Takes an array of objects and uses the class and id of the first
    # one to create an attributes hash.
    # The second object, if present, is used as a prefix,
    # just like you can do with `dom_id()` and `dom_class()` in Rails
    def parse_object_ref(ref)
      prefix = ref[1]
      ref = ref[0]
      # Let's make sure the value isn't nil. If it is, return the default Hash.
      return {} if ref.nil?
      class_name =
        if ref.respond_to?(:haml_object_ref)
          ref.haml_object_ref
        else
          underscore(ref.class)
        end
      id = "#{class_name}_#{ref.id || 'new'}"
      if prefix
        class_name = "#{ prefix }_#{ class_name}"
        id = "#{ prefix }_#{ id }"
      end

      {'id' => id, 'class' => class_name}
    end

    # Changes a word from camel case to underscores.
    # Based on the method of the same name in Rails' Inflector,
    # but copied here so it'll run properly without Rails.
    def underscore(camel_cased_word)
      camel_cased_word.to_s.gsub(/::/, '_').
        gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
        gsub(/([a-z\d])([A-Z])/,'\1_\2').
        tr("-", "_").
        downcase
    end
  end
end
