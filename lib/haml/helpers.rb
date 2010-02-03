if defined?(ActionView)
  require 'haml/helpers/action_view_mods'
  require 'haml/helpers/action_view_extensions'
end

module Haml
  # This module contains various helpful methods to make it easier to do various tasks.
  # {Haml::Helpers} is automatically included in the context
  # that a Haml template is parsed in, so all these methods are at your
  # disposal from within the template.
  module Helpers
    # An object that raises an error when \{#to\_s} is called.
    # It's used to raise an error when the return value of a helper is used
    # when it shouldn't be.
    class ErrorReturn
      # @param message [String] The error message to raise when \{#to\_s} is called
      def initialize(method)
        @message = <<MESSAGE
#{method} outputs directly to the Haml template.
Disregard its return value and use the - operator,
or use capture_haml to get the value as a String.
MESSAGE
      end

      # Raises an error.
      #
      # @raise [Haml::Error] The error
      def to_s
        raise Haml::Error.new(@message)
      rescue Haml::Error => e
        e.backtrace.shift

        # If the ErrorReturn is used directly in the template,
        # we don't want Haml's stuff to get into the backtrace,
        # so we get rid of the format_script line.
        #
        # We also have to subtract one from the Haml line number
        # since the value is passed to format_script the line after
        # it's actually used.
        if e.backtrace.first =~ /^\(eval\):\d+:in `format_script/
          e.backtrace.shift
          e.backtrace.first.gsub!(/^\(haml\):(\d+)/) {|s| "(haml):#{$1.to_i - 1}"}
        end
        raise e
      end

      # @return [String] A human-readable string representation
      def inspect
        "Haml::Helpers::ErrorReturn(#{@message.inspect})"
      end
    end

    self.extend self

    @@action_view_defined = defined?(ActionView)
    @@force_no_action_view = false

    # @return [Boolean] Whether or not ActionView is loaded
    def self.action_view?
      @@action_view_defined
    end

    # Note: this does **not** need to be called when using Haml helpers
    # normally in Rails.
    #
    # Initializes the current object as though it were in the same context
    # as a normal ActionView instance using Haml.
    # This is useful if you want to use the helpers in a context
    # other than the normal setup with ActionView.
    # For example:
    #
    #     context = Object.new
    #     class << context
    #       include Haml::Helpers
    #     end
    #     context.init_haml_helpers
    #     context.haml_tag :p, "Stuff"
    #
    def init_haml_helpers
      @haml_buffer = Haml::Buffer.new(@haml_buffer, Haml::Engine.new('').send(:options_for_buffer))
      nil
    end

    # Runs a block of code in a non-Haml context
    # (i.e. \{#is\_haml?} will return false).
    #
    # This is mainly useful for rendering sub-templates such as partials in a non-Haml language,
    # particularly where helpers may behave differently when run from Haml.
    #
    # Note that this is automatically applied to Rails partials.
    #
    # @yield A block which won't register as Haml
    def non_haml
      was_active = @haml_buffer.active?
      @haml_buffer.active = false
      yield
    ensure
      @haml_buffer.active = was_active
    end

    # Uses \{#preserve} to convert any newlines inside whitespace-sensitive tags
    # into the HTML entities for endlines.
    #
    # @param tags [Array<String>] Tags that should have newlines escaped
    #
    # @overload find_and_preserve(input, tags = haml_buffer.options[:preserve])
    #   Escapes newlines within a string.
    #
    #   @param input [String] The string within which to escape newlines
    # @overload find_and_preserve(tags = haml_buffer.options[:preserve])
    #   Escapes newlines within a block of Haml code.
    #
    #   @yield The block within which to escape newlines
    def find_and_preserve(input = nil, tags = haml_buffer.options[:preserve], &block)
      return find_and_preserve(capture_haml(&block), input || tags) if block
      input.to_s.gsub(/<(#{tags.map(&Regexp.method(:escape)).join('|')})([^>]*)>(.*?)(<\/\1>)/im) do
        "<#{$1}#{$2}>#{preserve($3)}</#{$1}>"
      end
    end

    # Takes any string, finds all the newlines, and converts them to
    # HTML entities so they'll render correctly in
    # whitespace-sensitive tags without screwing up the indentation.
    #
    # @overload perserve(input)
    #   Escapes newlines within a string.
    #
    #   @param input [String] The string within which to escape all newlines
    # @overload perserve
    #   Escapes newlines within a block of Haml code.
    #
    #   @yield The block within which to escape newlines
    def preserve(input = nil, &block)
      return preserve(capture_haml(&block)) if block
      input.to_s.chomp("\n").gsub(/\n/, '&#x000A;').gsub(/\r/, '')
    end
    alias_method :flatten, :preserve

    # Takes an `Enumerable` object and a block
    # and iterates over the enum,
    # yielding each element to a Haml block
    # and putting the result into `<li>` elements.
    # This creates a list of the results of the block.
    # For example:
    #
    #     = list_of([['hello'], ['yall']]) do |i|
    #       = i[0]
    #
    # Produces:
    #
    #     <li>hello</li>
    #     <li>yall</li>
    #
    # And
    #
    #     = list_of({:title => 'All the stuff', :description => 'A book about all the stuff.'}) do |key, val|
    #       %h3= key.humanize
    #       %p= val
    #
    # Produces:
    #
    #     <li>
    #       <h3>Title</h3>
    #       <p>All the stuff</p>
    #     </li>
    #     <li>
    #       <h3>Description</h3>
    #       <p>A book about all the stuff.</p>
    #     </li>
    #
    # @param enum [Enumerable] The list of objects to iterate over
    # @yield [item] A block which contains Haml code that goes within list items
    # @yieldparam item An element of `enum`
    def list_of(enum, &block)
      to_return = enum.collect do |i|
        result = capture_haml(i, &block)

        if result.count("\n") > 1
          result.gsub!("\n", "\n  ")
          result = "\n  #{result.strip}\n"
        else
          result.strip!
        end

        "<li>#{result}</li>"
      end
      to_return.join("\n")
    end

    # Returns a hash containing default assignments for the `xmlns`, `lang`, and `xml:lang`
    # attributes of the `html` HTML element.
    # For example,
    #
    #     %html{html_attrs}
    #
    # becomes
    #
    #     <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en-US' lang='en-US'>
    #
    # @param lang [String] The value of `xml:lang` and `lang`
    # @return [{#to_s => String}] The attribute hash
    def html_attrs(lang = 'en-US')
      {:xmlns => "http://www.w3.org/1999/xhtml", 'xml:lang' => lang, :lang => lang}
    end

    # Increments the number of tabs the buffer automatically adds
    # to the lines of the template.
    # For example:
    #
    #     %h1 foo
    #     - tab_up
    #     %p bar
    #     - tab_down
    #     %strong baz
    #
    # Produces:
    #
    #     <h1>foo</h1>
    #       <p>bar</p>
    #     <strong>baz</strong>
    #
    # @param i [Fixnum] The number of tabs by which to increase the indentation
    # @see #tab_down
    def tab_up(i = 1)
      haml_buffer.tabulation += i
    end

    # Decrements the number of tabs the buffer automatically adds
    # to the lines of the template.
    #
    # @param i [Fixnum] The number of tabs by which to decrease the indentation
    # @see #tab_up
    def tab_down(i = 1)
      haml_buffer.tabulation -= i
    end

    # Surrounds a block of Haml code with strings,
    # with no whitespace in between.
    # For example:
    #
    #     = surround '(', ')' do
    #       %a{:href => "food"} chicken
    #
    # Produces:
    #
    #     (<a href='food'>chicken</a>)
    #
    # and
    #
    #     = surround '*' do
    #       %strong angry
    #
    # Produces:
    #
    #     *<strong>angry</strong>*
    #
    # @param front [String] The string to add before the Haml
    # @param back [String] The string to add after the Haml
    # @yield A block of Haml to surround
    def surround(front, back = front, &block)
      output = capture_haml(&block)

      "#{front}#{output.chomp}#{back}\n"
    end

    # Prepends a string to the beginning of a Haml block,
    # with no whitespace between.
    # For example:
    #
    #     = precede '*' do
    #       %span.small Not really
    #
    # Produces:
    #
    #     *<span class='small'>Not really</span>
    #
    # @param str [String] The string to add before the Haml
    # @yield A block of Haml to prepend to
    def precede(str, &block)
      "#{str}#{capture_haml(&block).chomp}\n"
    end

    # Appends a string to the end of a Haml block,
    # with no whitespace between.
    # For example:
    #
    #     click
    #     = succeed '.' do
    #       %a{:href=>"thing"} here
    #
    # Produces:
    #
    #     click
    #     <a href='thing'>here</a>.
    #
    # @param str [String] The string to add after the Haml
    # @yield A block of Haml to append to
    def succeed(str, &block)
      "#{capture_haml(&block).chomp}#{str}\n"
    end

    # Captures the result of a block of Haml code,
    # gets rid of the excess indentation,
    # and returns it as a string.
    # For example, after the following,
    #
    #     .foo
    #       - foo = capture_haml(13) do |a|
    #         %p= a
    #
    # the local variable `foo` would be assigned to `"<p>13</p>\n"`.
    #
    # @param args [Array] Arguments to pass into the block
    # @yield [args] A block of Haml code that will be converted to a string
    # @yieldparam args [Array] `args`
    def capture_haml(*args, &block)
      buffer = eval('_hamlout', block.binding) rescue haml_buffer
      with_haml_buffer(buffer) do
        position = haml_buffer.buffer.length

        haml_buffer.capture_position = position
        block.call(*args)

        captured = haml_buffer.buffer.slice!(position..-1).split(/^/)

        min_tabs = nil
        captured.each do |line|
          tabs = line.index(/[^ ]/) || line.length
          min_tabs ||= tabs
          min_tabs = min_tabs > tabs ? tabs : min_tabs
        end

        captured.map do |line|
          line[min_tabs..-1]
        end.join
      end
    ensure
      haml_buffer.capture_position = nil
    end

    # @deprecated This will be removed in version 3.0.
    # @see #haml_concat
    def puts(*args)
      warn <<END
DEPRECATION WARNING:
The Haml #puts helper is deprecated and will be removed in version 3.0.
Use the #haml_concat helper instead.
END
      haml_concat(*args)
    end

    # Outputs text directly to the Haml buffer, with the proper indentation.
    #
    # @param text [#to_s] The text to output
    def haml_concat(text = "")
      haml_buffer.buffer << haml_indent << text.to_s << "\n"
      ErrorReturn.new("haml_concat")
    end

    # @return [String] The indentation string for the current line
    def haml_indent
      '  ' * haml_buffer.tabulation
    end

    # Creates an HTML tag with the given name and optionally text and attributes.
    # Can take a block that will run between the opening and closing tags.
    # If the block is a Haml block or outputs text using \{#haml\_concat},
    # the text will be properly indented.
    #
    # `flags` is a list of symbol flags
    # like those that can be put at the end of a Haml tag
    # (`:/`, `:<`, and `:>`).
    # Currently, only `:/` and `:<` are supported.
    #
    # `haml_tag` outputs directly to the buffer;
    # its return value should not be used.
    # If you need to get the results as a string,
    # use \{#capture\_haml\}.
    #
    # For example,
    #
    #     haml_tag :table do
    #       haml_tag :tr do
    #         haml_tag :td, {:class => 'cell'} do
    #           haml_tag :strong, "strong!"
    #           haml_concat "data"
    #         end
    #         haml_tag :td do
    #           haml_concat "more_data"
    #         end
    #       end
    #     end
    #
    # outputs
    #
    #     <table>
    #       <tr>
    #         <td class='cell'>
    #           <strong>
    #             strong!
    #           </strong>
    #           data
    #         </td>
    #         <td>
    #           more_data
    #         </td>
    #       </tr>
    #     </table>
    #
    # @param name [#to_s] The name of the tag
    # @param flags [Array<Symbol>] Haml end-of-tag flags
    #
    # @overload haml_tag(name, *flags, attributes = {})
    #   @yield The block of Haml code within the tag
    # @overload haml_tag(name, text, *flags, attributes = {})
    #   @param text [#to_s] The text within the tag
    def haml_tag(name, *rest, &block)
      ret = ErrorReturn.new("haml_tag")

      name = name.to_s
      text = rest.shift.to_s unless [Symbol, Hash, NilClass].any? {|t| rest.first.is_a? t}
      flags = []
      flags << rest.shift while rest.first.is_a? Symbol
      attributes = Haml::Precompiler.build_attributes(haml_buffer.html?,
                                                      haml_buffer.options[:attr_wrapper],
                                                      rest.shift || {})

      if text.nil? && block.nil? && (haml_buffer.options[:autoclose].include?(name) || flags.include?(:/))
        haml_concat "<#{name}#{attributes} />"
        return ret
      end

      if flags.include?(:/)
        raise Error.new("Self-closing tags can't have content.") if text
        raise Error.new("Illegal nesting: nesting within a self-closing tag is illegal.") if block
      end

      tag = "<#{name}#{attributes}>"
      if block.nil?
        tag << text.to_s << "</#{name}>"
        haml_concat tag
        return ret
      end

      if text
        raise Error.new("Illegal nesting: content can't be both given to haml_tag :#{name} and nested within it.")
      end

      if flags.include?(:<)
        tag << capture_haml(&block).strip << "</#{name}>"
        haml_concat tag
        return ret
      end

      haml_concat tag
      tab_up
      block.call
      tab_down
      haml_concat "</#{name}>"

      ret
    end

    # Characters that need to be escaped to HTML entities from user input
    # @private
    HTML_ESCAPE = { '&'=>'&amp;', '<'=>'&lt;', '>'=>'&gt;', '"'=>'&quot;', "'"=>'&#039;', }

    # Returns a copy of `text` with ampersands, angle brackets and quotes
    # escaped into HTML entities.
    #
    # Note that if ActionView is loaded and XSS protection is enabled
    # (as is the default for Rails 3.0+, and optional for version 2.3.5+),
    # this won't escape text declared as "safe".
    #
    # @param text [String] The string to sanitize
    # @return [String] The sanitized string
    def html_escape(text)
      text.to_s.gsub(/[\"><&]/n) {|s| HTML_ESCAPE[s]}
    end

    # Escapes HTML entities in `text`, but without escaping an ampersand
    # that is already part of an escaped entity.
    #
    # @param text [String] The string to sanitize
    # @return [String] The sanitized string
    def escape_once(text)
      Haml::Util.silence_warnings do
        text.to_s.gsub(/[\"><]|&(?!(?:[a-zA-Z]+|(#\d+));)/n) {|s| HTML_ESCAPE[s]}
      end
    end

    # Returns whether or not the current template is a Haml template.
    #
    # This function, unlike other {Haml::Helpers} functions,
    # also works in other `ActionView` templates,
    # where it will always return false.
    #
    # @return [Boolean] Whether or not the current template is a Haml template
    def is_haml?
      !@haml_buffer.nil? && @haml_buffer.active?
    end

    # Returns whether or not `block` is defined directly in a Haml template.
    #
    # @param block [Proc] A Ruby block
    # @return [Boolean] Whether or not `block` is defined directly in a Haml template
    def block_is_haml?(block)
      eval('_hamlout', block.binding)
      true
    rescue
      false
    end

    private

    # Runs a block of code with the given buffer as the currently active buffer.
    #
    # @param buffer [Haml::Buffer] The Haml buffer to use temporarily
    # @yield A block in which the given buffer should be used
    def with_haml_buffer(buffer)
      @haml_buffer, old_buffer = buffer, @haml_buffer
      old_buffer.active, was_active = false, old_buffer.active? if old_buffer
      @haml_buffer.active = true
      yield
    ensure
      @haml_buffer.active = false
      old_buffer.active = was_active if old_buffer
      @haml_buffer = old_buffer
    end

    # The current {Haml::Buffer} object.
    #
    # @return [Haml::Buffer]
    def haml_buffer
      @haml_buffer
    end

    # Gives a proc the same local `_hamlout` and `_erbout` variables
    # that the current template has.
    #
    # @param proc [#call] The proc to bind
    # @return [Proc] A new proc with the new variables bound
    def haml_bind_proc(&proc)
      _hamlout = haml_buffer
      _erbout = _hamlout.buffer
      proc { |*args| proc.call(*args) }
    end

    include ActionViewExtensions if self.const_defined? "ActionViewExtensions"
  end
end

class Object
  # Haml overrides various `ActionView` helpers,
  # which call an \{#is\_haml?} method
  # to determine whether or not the current context object
  # is a proper Haml context.
  # Because `ActionView` helpers may be included in non-`ActionView::Base` classes,
  # it's a good idea to define \{#is\_haml?} for all objects.
  def is_haml?
    false
  end
end

