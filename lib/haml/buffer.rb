module Haml
  # This class is used only internally. It holds the buffer of XHTML that
  # is eventually output by Haml::Engine's to_html method. It's called
  # from within the precompiled code, and helps reduce the amount of
  # processing done within instance_eval'd code.
  class Buffer
    include Haml::Helpers

    # The string that holds the compiled XHTML. This is aliased as
    # _erbout for compatibility with ERB-specific code.
    attr_accessor :buffer

    # The options hash passed in from Haml::Engine.
    attr_accessor :options

    # The Buffer for the enclosing Haml document.
    # This is set for partials and similar sorts of nested templates.
    # It's nil at the top level (see #toplevel?).
    attr_accessor :upper

    # See #active?
    attr_writer :active

    # True if the format is XHTML
    def xhtml?
      not html?
    end

    # True if the format is any flavor of HTML
    def html?
      html4? or html5?
    end

    # True if the format is HTML4
    def html4?
      @options[:format] == :html4
    end

    # True if the format is HTML5
    def html5?
      @options[:format] == :html5
    end

    # True if this buffer is a top-level template,
    # as opposed to a nested partial.
    def toplevel?
      upper.nil?
    end

    # True if this buffer is currently being used to render a Haml template.
    # However, this returns false if a subtemplate is being rendered,
    # even if it's a subtemplate of this buffer's template.
    def active?
      @active
    end

    # Gets the current tabulation of the document.
    def tabulation
      @real_tabs + @tabulation
    end

    # Sets the current tabulation of the document.
    def tabulation=(val)
      val = val - @real_tabs
      @tabulation = val > -1 ? val : 0
    end

    # Creates a new buffer.
    def initialize(upper = nil, options = {})
      @active = true
      @upper = upper
      @options = {
        :attr_wrapper => "'",
        :ugly => false,
        :format => :xhtml
      }.merge options
      @buffer = ""
      @tabulation = 0

      # The number of tabs that Engine thinks we should have
      # @real_tabs + @tabulation is the number of tabs actually output
      @real_tabs = 0
    end

    # Renders +text+ with the proper tabulation. This also deals with
    # making a possible one-line tag one line or not.
    def push_text(text, tab_change = 0)
      if @tabulation > 0 && !@options[:ugly]
        # Have to push every line in by the extra user set tabulation
        text.gsub!(/^/m, '  ' * @tabulation)
      end

      @buffer << text
      @real_tabs += tab_change
    end

    # Properly formats the output of a script that was run in the
    # instance_eval.
    def push_script(result, preserve_script, close_tag = nil, preserve_tag = false, escape_html = false)
      tabulation = @real_tabs

      if preserve_tag
        result = Haml::Helpers.preserve(result)
      elsif preserve_script
        result = Haml::Helpers.find_and_preserve(result)
      end

      result = result.to_s
      while result[-1] == ?\n
        # String#chomp is slow
        result = result[0...-1]
      end

      result = html_escape(result) if escape_html

      if close_tag && (@options[:ugly] || !result.include?("\n") || preserve_tag)
        @buffer << "#{result}</#{close_tag}>\n"
        @real_tabs -= 1
      else
        if close_tag
          @buffer << "\n"
        end

        result = result.gsub(/^/m, tabs(tabulation)) unless @options[:ugly]
        @buffer << "#{result}\n"

        if close_tag
          # We never get here if @options[:ugly] is true
          @buffer << "#{tabs(tabulation-1)}</#{close_tag}>\n"
          @real_tabs -= 1
        end
      end
      nil
    end

    # Takes the various information about the opening tag for an
    # element, formats it, and adds it to the buffer.
    def open_tag(name, self_closing, try_one_line, preserve_tag, escape_html, class_id, obj_ref, content, *attributes_hashes)
      tabulation = @real_tabs

      attributes = class_id
      attributes_hashes.each do |attributes_hash|
        attributes_hash.keys.each { |key| attributes_hash[key.to_s] = attributes_hash.delete(key) }
        self.class.merge_attrs(attributes, attributes_hash)
      end
      self.class.merge_attrs(attributes, parse_object_ref(obj_ref)) if obj_ref

      if self_closing
        str = " />\n"
      elsif try_one_line || preserve_tag
        str = ">"
      else
        str = ">\n"
      end

      attributes = Precompiler.build_attributes(html?, @options[:attr_wrapper], attributes)
      @buffer << "#{@options[:ugly] ? '' : tabs(tabulation)}<#{name}#{attributes}#{str}"

      if content
        if @options[:ugly] || !content.include?("\n")
          @buffer << "#{content}</#{name}>\n"
        else
          @buffer << "\n#{tabs(@real_tabs+1)}#{content}\n#{tabs(@real_tabs)}</#{name}>\n"
        end
      elsif !self_closing
        @real_tabs += 1
      end
    end

    def self.merge_attrs(to, from)
      if to['id'] && from['id']
        to['id'] << '_' << from.delete('id')
      elsif to['id'] || from['id']
        from['id'] ||= to['id']
      end

      if to['class'] && from['class']
        # Make sure we don't duplicate class names
        from['class'] = (from['class'].split(' ') | to['class'].split(' ')).join(' ')
      elsif to['class'] || from['class']
        from['class'] ||= to['class']
      end

      to.merge!(from)
    end

    private

    # Some of these methods are exposed as public class methods
    # so they can be re-used in helpers.

    @@tab_cache = {}
    # Gets <tt>count</tt> tabs. Mostly for internal use.
    def tabs(count)
      tabs = count + @tabulation
      @@tab_cache[tabs] ||= '  ' * tabs
    end

    # Takes an array of objects and uses the class and id of the first
    # one to create an attributes hash.
    # The second object, if present, is used as a prefix,
    # just like you can do with dom_id() and dom_class() in Rails
    def parse_object_ref(ref)
      prefix = ref[1]
      ref = ref[0]
      # Let's make sure the value isn't nil. If it is, return the default Hash.
      return {} if ref.nil?
      class_name = underscore(ref.class)
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
