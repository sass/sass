require File.dirname(__FILE__) + '/helpers'
require File.dirname(__FILE__) + '/buffer'

module Haml
  # This is the class where all the parsing and processing of the HAML
  # template is done. It can be directly used by the user by creating a
  # new instance and calling to_html to render the template. For example:
  #
  #   template = File.load('templates/really_cool_template.haml')
  #   haml_engine = Haml::Engine.new(template)
  #   output = haml_engine.to_html
  #   puts output
  class Engine
    # Allow access to the precompiled template
    attr_reader :precompiled

    # Allow reading and writing of the options hash
    attr :options, true

    # Designates an XHTML/XML element.
    ELEMENT         = '%'[0]

    # Designates a <tt><div></tt> element with the given class.
    DIV_CLASS       = '.'[0]

    # Designates a <tt><div></tt> element with the given id.
    DIV_ID          = '#'[0]

    # Designates an XHTML/XML comment.
    COMMENT         = '/'[0]

    # Designates an XHTML doctype.
    DOCTYPE         = '!'[0]

    # Designates script, the result of which is output.
    SCRIPT          = '='[0]

    # Designates script, the result of which is flattened and output.
    FLAT_SCRIPT     = '~'[0]

    # Designates script which is run but not output.
    SILENT_SCRIPT   = '-'[0]

    # When following SILENT_SCRIPT, designates a comment that is not output.
    SILENT_COMMENT  = '#'[0]

    # Designates a non-parsed line.
    ESCAPE          = '\\'[0]

    # Designates a non-parsed line. Not actually a character.
    PLAIN_TEXT      = -1

    # Keeps track of the ASCII values of the characters that begin a
    # specially-interpreted line.
    SPECIAL_CHARACTERS   = [
      ELEMENT,
      DIV_CLASS,
      DIV_ID,
      COMMENT,
      DOCTYPE,
      SCRIPT,
      FLAT_SCRIPT,
      SILENT_SCRIPT,
      ESCAPE
    ]

    # The value of the character that designates that a line is part
    # of a multiline string.
    MULTILINE_CHAR_VALUE = '|'[0]

    # Characters that designate that a multiline string may be about
    # to begin.
    MULTILINE_STARTERS   = SPECIAL_CHARACTERS - ["/"[0]]

    # Keywords that appear in the middle of a Ruby block with lowered
    # indentation. If a block has been started using indentation,
    # lowering the indentation  with one of these won't end the block.
    # For example:
    #
    #   - if foo
    #     %p yes!
    #   - else
    #     %p no!
    #
    # The block is ended after <tt>%p no!</tt>, because <tt>else</tt>
    # is a member of this array.
    MID_BLOCK_KEYWORDS   = ['else', 'elsif', 'rescue', 'ensure', 'when']

    # Creates a new instace of Haml::Engine that will compile the given
    # template string when <tt>to_html</tt> is called.
    # See REFERENCE for available options.
    #
    #--
    # When adding options, remember to add information about them
    # to REFERENCE!
    #++
    #
    def initialize(template, options = {})
      @options = {
        :suppress_eval => false,
        :attr_wrapper => "'",
        :locals => {}
      }.merge options
      @precompiled = @options[:precompiled]

      @template = template.strip #String
      @to_close_stack = []
      @output_tabs = 0
      @template_tabs = 0

      # This is the base tabulation of the currently active
      # flattened block. -1 signifies that there is no such block.
      @flat_spaces = -1

      # Only do the first round of pre-compiling if we really need to.
      # They might be passing in the precompiled string.
      do_precompile if @precompiled.nil? && (@precompiled = String.new)
    end

    # Processes the template and returns the result as a string.
    def to_html(scope = Object.new, &block)
      @scope_object = scope
      @buffer = Haml::Buffer.new(@options)

      local_assigns = @options[:locals]

      # Get inside the view object's world
      @scope_object.instance_eval do
        # Set all the local assigns
        local_assigns.each do |key,val|
          self.class.send(:define_method, key) { val }
        end
      end

      # Compile the @precompiled buffer
      compile &block

      # Return the result string
      @buffer.buffer
    end

   private

    #Precompile each line
    def do_precompile
      push_silent <<-END
        def _haml_render
        _hamlout = @haml_stack[-1]
        _erbout = _hamlout.buffer
      END
      
      old_line = nil
      old_index = nil
      old_spaces = nil
      old_tabs = nil
      (@template + "\n-#").each_with_index do |line, index|
        spaces, tabs = count_soft_tabs(line)
        line = line.strip
        
        if !line.empty?
          if old_line
            block_opened = tabs > old_tabs && !line.empty?
            
            suppress_render = handle_multiline(old_tabs, old_line, old_index)
            
            if !suppress_render
              line_empty = old_line.empty?
              process_indent(old_tabs, old_line) unless line_empty
              flat = @flat_spaces != -1

              if flat
                push_flat(old_line, old_spaces)
              elsif !line_empty
                process_line(old_line, old_index, block_opened)
              end
            end
          end
          
          old_line = line
          old_index = index
          old_spaces = spaces
          old_tabs = tabs
        elsif @flat_spaces != -1
          push_flat(old_line, old_spaces)
          old_line = ''
          old_spaces = 0
        end
      end

      # Close all the open tags
      @template_tabs.times { close }

      push_silent "end"
    end
    
    # Processes and deals with lowering indentation.
    def process_indent(count, line)
      if count <= @template_tabs && @template_tabs > 0
        to_close = @template_tabs - count

        to_close.times do |i|
          offset = to_close - 1 - i
          unless offset == 0 && mid_block_keyword?(line)
            close
          end
        end
      end
    end

    # Processes a single line of HAML.
    #
    # This method doesn't return anything; it simply processes the line and
    # adds the appropriate code to <tt>@precompiled</tt>.
    def process_line(line, index, block_opened)
      case line[0]
      when DIV_CLASS, DIV_ID
        render_div(line, index)
      when ELEMENT
        render_tag(line, index)
      when COMMENT
        render_comment(line)
      when SCRIPT
        push_script(line[1..-1], false, block_opened, index)
      when FLAT_SCRIPT
        push_flat_script(line[1..-1], block_opened, index)
      when SILENT_SCRIPT
        sub_line = line[1..-1]
        unless sub_line[0] == SILENT_COMMENT
          push_silent(sub_line, index)
          if block_opened && !mid_block_keyword?(line)
            push_and_tabulate([:script])
          end
        end
      when DOCTYPE
        if line[0...3] == '!!!'
          render_doctype(line)
        else
          push_text line
        end
      when ESCAPE
        push_text line[1..-1]
      else
        push_text line
      end
    end
    
    # Returns whether or not the line is a silent script line with one
    # of Ruby's mid-block keywords.
    def mid_block_keyword?(line)
      line.length > 2 && line[0] == SILENT_SCRIPT && MID_BLOCK_KEYWORDS.include?(line[1..-1].split[0])
    end

    # Deals with all the logic of figuring out whether a given line is
    # the beginning, continuation, or end of a multiline sequence.
    #
    # This returns whether or not the line should be
    # rendered normally.
    def handle_multiline(count, line, index)
      suppress_render = false
      # Multilines are denoting by ending with a `|` (124)
      if is_multiline?(line) && @multiline_buffer
        # A multiline string is active, and is being continued
        @multiline_buffer += line[0...-1]
        suppress_render = true
      elsif is_multiline?(line) && (MULTILINE_STARTERS.include? line[0])
        # A multiline string has just been activated, start adding the lines
        @multiline_buffer = line[0...-1]
        @multiline_count = count
        @multiline_index = index
        process_indent(count, line)
        suppress_render = true
      elsif @multiline_buffer
        # A multiline string has just ended, make line into the result
        unless line.empty?
          process_line(@multiline_buffer, @multiline_index, count > @multiline_count)
          @multiline_buffer = nil
        end
      end

      return suppress_render
    end

    # Checks whether or not +line+ is in a multiline sequence.
    def is_multiline?(line)                                          # ' '[0] == 32
      line && line.length > 1 && line[-1] == MULTILINE_CHAR_VALUE && line[-2] == 32
    end

    # Takes <tt>@precompiled</tt>, a string buffer of Ruby code, and
    # evaluates it in the context of <tt>@scope_object</tt>, after preparing
    # <tt>@scope_object</tt>. The code in <tt>@precompiled</tt> populates
    # <tt>@buffer</tt> with the compiled XHTML code.
    def compile(&block)
      # Set the local variables pointing to the buffer
      buffer = @buffer
      @scope_object.extend Haml::Helpers
      @scope_object.instance_eval do
        @haml_stack ||= Array.new
        @haml_stack.push(buffer)

        class << self
          attr :haml_lineno # :nodoc:
        end
      end

      begin
        # Evaluate the buffer in the context of the scope object
        @scope_object.instance_eval @precompiled
        @scope_object._haml_render &block
      rescue Exception => e
        # Get information from the exception and format it so that
        # Rails can understand it.
        compile_error = e.message.scan(/\(eval\):([0-9]*):in `[-_a-zA-Z]*': compile error/)[0]
        filename = "(haml)"
        if @scope_object.methods.include? "haml_filename"
          # For some reason that I can't figure out,
          # @scope_object.methods.include? "haml_filename" && @scope_object.haml_filename
          # is false when it shouldn't be. Nested if statements work, though.

          if @scope_object.haml_filename
            filename = "#{@scope_object.haml_filename}.haml"
          end
        end
        lineno = @scope_object.haml_lineno

        if compile_error
          eval_line = compile_error[0].to_i
          line_marker = @precompiled.split("\n")[0...eval_line].grep(/@haml_lineno = [0-9]*/)[-1]
          lineno = line_marker.scan(/[0-9]+/)[0].to_i if line_marker
        end

        e.backtrace.unshift "#{filename}:#{lineno}"
        raise e
      end

      # Get rid of the current buffer
      @scope_object.instance_eval do
        @haml_stack.pop
      end
    end

    # Evaluates <tt>text</tt> in the context of <tt>@scope_object</tt>, but
    # does not output the result.
    def push_silent(text, index = nil)
      if index
        @precompiled << "@haml_lineno = #{index + 1}\n#{text}\n"
      else
        # Not really DRY, but probably faster
        @precompiled << "#{text}\n"
      end
    end

    # Adds <tt>text</tt> to <tt>@buffer</tt> with appropriate tabulation
    # without parsing it.
    def push_text(text)
      @precompiled << "_hamlout.push_text(#{text.dump}, #{@output_tabs})\n"
    end

    # Adds +text+ to <tt>@buffer</tt> while flattening text.
    def push_flat(text, spaces)
      tabulation = spaces - @flat_spaces
      @precompiled << "_hamlout.push_text(#{text.dump}, #{tabulation > -1 ? tabulation : 0}, true)\n"
    end

    # Causes <tt>text</tt> to be evaluated in the context of
    # <tt>@scope_object</tt> and the result to be added to <tt>@buffer</tt>.
    #
    # If <tt>flattened</tt> is true, Haml::Helpers#find_and_flatten is run on
    # the result before it is added to <tt>@buffer</tt>
    def push_script(text, flattened, block_opened, index)
      unless options[:suppress_eval]
        push_silent("haml_temp = #{text}", index)
        out = "haml_temp = _hamlout.push_script(haml_temp, #{@output_tabs}, #{flattened})\n"
        if block_opened
          push_and_tabulate([:loud, out])
        else
          @precompiled << out
        end
      end
    end
    
    # Causes <tt>text</tt> to be evaluated, and Haml::Helpers#find_and_flatten
    # to be run on it afterwards.
    def push_flat_script(text, block_opened, index)
      unless text.empty?
        push_script(text, true, block_opened, index)
      else
        start_flat(false)
      end
    end

    # Closes the most recent item in <tt>@to_close_stack</tt>.
    def close
      tag, value = @to_close_stack.pop
      case tag
      when :script
        close_block
      when :comment
        close_comment value
      when :element
        close_tag value
      when :flat
        close_flat value
      when :loud
        close_loud value
      end
    end

    # Puts a line in <tt>@precompiled</tt> that will add the closing tag of
    # the most recently opened tag.
    def close_tag(tag)
      @output_tabs -= 1
      @template_tabs -= 1
      @precompiled << "_hamlout.close_tag(#{tag.dump}, #{@output_tabs})\n"
    end

    # Closes a Ruby block.
    def close_block
      push_silent "end"
      @template_tabs -= 1
    end

    # Closes a comment.
    def close_comment(has_conditional)
      @output_tabs -= 1
      @template_tabs -= 1
      push_silent "_hamlout.close_comment(#{has_conditional}, #{@output_tabs})"
    end
    
    # Closes a flattened section.
    def close_flat(in_tag)
      @flat_spaces = -1
      if in_tag
        close
      else
        push_silent('_hamlout.stop_flat')
        @template_tabs -= 1
      end
    end
    
    # Closes a loud Ruby block.
    def close_loud(command)
      push_silent "end"
      @precompiled << command
      @template_tabs -= 1
    end

    # Parses a line that will render as an XHTML tag, and adds the code that will
    # render that tag to <tt>@precompiled</tt>.
    def render_tag(line, index)
      line.scan(/[%]([-:_a-zA-Z0-9]+)([-_a-zA-Z0-9\.\#]*)(\{.*\})?(\[.*\])?([=\/\~]?)?(.*)?/) do |tag_name, attributes, attributes_hash, object_ref, action, value|
        value = value.to_s

        case action
        when '/'
          atomic = true
        when '=', '~'
          parse = true
        else
          value = value.strip
        end

        flattened = (action == '~')
        value_exists = !value.empty?
        attributes_hash = "nil" unless attributes_hash
        object_ref = "nil" unless object_ref

        push_silent "_hamlout.open_tag(#{tag_name.inspect}, #{@output_tabs}, #{atomic.inspect}, #{value_exists.inspect}, #{attributes.inspect}, #{attributes_hash}, #{object_ref}, #{flattened.inspect})"

        unless atomic
          push_and_tabulate([:element, tag_name])
          @output_tabs += 1

          if value_exists
            if parse
              push_script(value, flattened, false, index)
            else
              push_text(value)
            end
            close
          elsif flattened
            start_flat(true)
          end
        end
      end
    end

    # Renders a line that creates an XHTML tag and has an implicit div because of
    # <tt>.</tt> or <tt>#</tt>.
    def render_div(line, index)
      render_tag('%div' + line, index)
    end

    # Renders an XHTML comment.
    def render_comment(line)
      conditional, content = line.scan(/\/(\[[a-zA-Z0-9 \.]*\])?(.*)/)[0]
      content = content.strip
      try_one_line = !content.empty?
      push_silent "_hamlout.open_comment(#{try_one_line}, #{conditional.inspect}, #{@output_tabs})"
      @output_tabs += 1
      push_and_tabulate([:comment, !conditional.nil?])
      if try_one_line
        push_text content
        close
      end
    end
    
    # Renders an XHTML doctype or XML shebang.
    def render_doctype(line)
      line = line[3..-1].lstrip.downcase
      if line[0...3] == "xml"
        encoding = line.split[1] || "utf-8"
        wrapper = @options[:attr_wrapper]
        doctype = "<?xml version=#{wrapper}1.0#{wrapper} encoding=#{wrapper}#{encoding}#{wrapper} ?>"
      else
        version, type = line.scan(/([0-9]\.[0-9])?[\s]*([a-zA-Z]*)/)[0]
        if version == "1.1"
          doctype = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">'
        else
          case type
          when "strict"
            doctype = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'
          when "frameset"
            doctype = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">'
          else
            doctype = '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
          end
        end
      end
      push_text doctype
    end
    
    # Starts a flattened block.
    def start_flat(in_tag)
      # @flat_spaces is the number of indentations in the template
      # that forms the base of the flattened area
      if in_tag
        @to_close_stack.push([:flat, true])
      else
        push_and_tabulate([:flat])
      end
      @flat_spaces = @template_tabs * 2
    end

    # Counts the tabulation of a line.
    def count_soft_tabs(line)
      spaces = line.index(/[^ ]/)
      spaces ? [spaces, spaces/2] : []
    end
    
    # Pushes value onto <tt>@to_close_stack</tt> and increases
    # <tt>@template_tabs</tt>.
    def push_and_tabulate(value)
      @to_close_stack.push(value)
      @template_tabs += 1
    end
  end
end
