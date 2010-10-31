require 'strscan'
require 'haml/shared'

module Haml
  # Handles the internal pre-compilation from Haml into Ruby code,
  # which then runs the final creation of the HTML string.
  module Precompiler
    include Haml::Util

    # Designates an XHTML/XML element.
    ELEMENT         = ?%

    # Designates a `<div>` element with the given class.
    DIV_CLASS       = ?.

    # Designates a `<div>` element with the given id.
    DIV_ID          = ?#

    # Designates an XHTML/XML comment.
    COMMENT         = ?/

    # Designates an XHTML doctype or script that is never HTML-escaped.
    DOCTYPE         = ?!

    # Designates script, the result of which is output.
    SCRIPT          = ?=

    # Designates script that is always HTML-escaped.
    SANITIZE        = ?&

    # Designates script, the result of which is flattened and output.
    FLAT_SCRIPT     = ?~

    # Designates script which is run but not output.
    SILENT_SCRIPT   = ?-

    # When following SILENT_SCRIPT, designates a comment that is not output.
    SILENT_COMMENT  = ?#

    # Designates a non-parsed line.
    ESCAPE          = ?\\

    # Designates a block of filtered text.
    FILTER          = ?:

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
      SANITIZE,
      FLAT_SCRIPT,
      SILENT_SCRIPT,
      ESCAPE,
      FILTER
    ]

    # The value of the character that designates that a line is part
    # of a multiline string.
    MULTILINE_CHAR_VALUE = ?|

    # Regex to match keywords that appear in the middle of a Ruby block
    # with lowered indentation.
    # If a block has been started using indentation,
    # lowering the indentation with one of these won't end the block.
    # For example:
    #
    #   - if foo
    #     %p yes!
    #   - else
    #     %p no!
    #
    # The block is ended after `%p no!`, because `else`
    # is a member of this array.
    MID_BLOCK_KEYWORD_REGEX = /^-\s*(#{%w[else elsif rescue ensure when end].join('|')})\b/

    # The Regex that matches a Doctype command.
    DOCTYPE_REGEX = /(\d(?:\.\d)?)?[\s]*([a-z]*)/i

    # The Regex that matches a literal string or symbol value
    LITERAL_VALUE_REGEX = /:(\w*)|(["'])((?![\\#]|\2).|\\.)*\2/

    private

    # Returns the precompiled string with the preamble and postamble
    def precompiled_with_ambles(local_names)
      preamble = <<END.gsub("\n", ";")
begin
extend Haml::Helpers
_hamlout = @haml_buffer = Haml::Buffer.new(@haml_buffer, #{options_for_buffer.inspect})
_erbout = _hamlout.buffer
__in_erb_template = true
END
      postamble = <<END.gsub("\n", ";")
#{precompiled_method_return_value}
ensure
@haml_buffer = @haml_buffer.upper
end
END
      preamble + locals_code(local_names) + precompiled + postamble
    end

    # Returns the string used as the return value of the precompiled method.
    # This method exists so it can be monkeypatched to return modified values.
    def precompiled_method_return_value
      "_erbout"
    end

    def locals_code(names)
      names = names.keys if Hash == names

      names.map do |name|
        # Can't use || because someone might explicitly pass in false with a symbol
        sym_local = "_haml_locals[#{inspect_obj(name.to_sym)}]"
        str_local = "_haml_locals[#{inspect_obj(name.to_s)}]"
        "#{name} = #{sym_local}.nil? ? #{str_local} : #{sym_local}"
      end.join(';') + ';'
    end

    # @private
    class Line < Struct.new(:text, :unstripped, :full, :index, :precompiler, :eod)
      alias_method :eod?, :eod

      # @private
      def tabs
        line = self
        @tabs ||= precompiler.instance_eval do
          break 0 if line.text.empty? || !(whitespace = line.full[/^\s+/])

          if @indentation.nil?
            @indentation = whitespace

            if @indentation.include?(?\s) && @indentation.include?(?\t)
              raise SyntaxError.new("Indentation can't use both tabs and spaces.", line.index)
            end

            @flat_spaces = @indentation * @template_tabs if flat?
            break 1
          end

          tabs = whitespace.length / @indentation.length
          break tabs if whitespace == @indentation * tabs
          break @template_tabs if flat? && whitespace =~ /^#{@indentation * @template_tabs}/

          raise SyntaxError.new(<<END.strip.gsub("\n", ' '), line.index)
Inconsistent indentation: #{Haml::Shared.human_indentation whitespace, true} used for indentation,
but the rest of the document was indented using #{Haml::Shared.human_indentation @indentation}.
END
        end
      end
    end

    def precompile
      @haml_comment = @dont_indent_next_line = @dont_tab_up_next_text = false
      @indentation = nil
      @line = next_line
      resolve_newlines
      newline

      raise SyntaxError.new("Indenting at the beginning of the document is illegal.", @line.index) if @line.tabs != 0

      while next_line
        process_indent(@line) unless @line.text.empty?

        if flat?
          push_flat(@line)
          @line = @next_line
          next
        end

        process_line(@line.text, @line.index) unless @line.text.empty? || @haml_comment

        if !flat? && @next_line.tabs - @line.tabs > 1
          raise SyntaxError.new("The line was indented #{@next_line.tabs - @line.tabs} levels deeper than the previous line.", @next_line.index)
        end

        resolve_newlines unless @next_line.eod?
        @line = @next_line
        newline unless @next_line.eod?
      end

      # Close all the open tags
      close until @to_close_stack.empty?
      flush_merged_text
    end

    # Processes and deals with lowering indentation.
    def process_indent(line)
      return unless line.tabs <= @template_tabs && @template_tabs > 0

      to_close = @template_tabs - line.tabs
      to_close.times {|i| close unless to_close - 1 - i == 0 && mid_block_keyword?(line.text)}
    end

    # Processes a single line of Haml.
    #
    # This method doesn't return anything; it simply processes the line and
    # adds the appropriate code to `@precompiled`.
    def process_line(text, index)
      @index = index + 1

      case text[0]
      when DIV_CLASS; render_div(text)
      when DIV_ID
        return push_plain(text) if text[1] == ?{
        render_div(text)
      when ELEMENT; render_tag(text)
      when COMMENT; render_comment(text[1..-1].strip)
      when SANITIZE
        return push_plain(text[3..-1].strip, :escape_html => true) if text[1..2] == "=="
        return push_script(text[2..-1].strip, :escape_html => true) if text[1] == SCRIPT
        return push_flat_script(text[2..-1].strip, :escape_html => true) if text[1] == FLAT_SCRIPT
        return push_plain(text[1..-1].strip, :escape_html => true) if text[1] == ?\s
        push_plain text
      when SCRIPT
        return push_plain(text[2..-1].strip) if text[1] == SCRIPT
        push_script(text[1..-1])
      when FLAT_SCRIPT; push_flat_script(text[1..-1])
      when SILENT_SCRIPT
        return start_haml_comment if text[1] == SILENT_COMMENT

        raise SyntaxError.new(<<END.rstrip, index) if text[1..-1].strip == "end"
You don't need to use "- end" in Haml. Un-indent to close a block:
- if foo?
  %strong Foo!
- else
  Not foo.
%p This line is un-indented, so it isn't part of the "if" block
END

        text = handle_ruby_multiline(text)
        push_silent(text[1..-1], true)
        newline_now

        # Handle stuff like - end.join("|")
        @to_close_stack.last << false if text =~ /^-\s*end\b/ && !block_opened?

        keyword = mid_block_keyword?(text)
        block = block_opened? && !keyword

        # It's important to preserve tabulation modification for keywords
        # that involve choosing between posible blocks of code.
        if %w[else elsif when].include?(keyword)
          # Whether a script block has already been opened immediately above this line
          was_opened = @to_close_stack.last && @to_close_stack.last.first == :script
          if was_opened
            @dont_indent_next_line, @dont_tab_up_next_text = @to_close_stack.last[1..2]
          end

          # when is unusual in that either it will be indented twice,
          # or the case won't have created its own indentation.
          # Also, if no block has been opened yet, we need to make sure we add an end
          # once we de-indent.
          if !was_opened || keyword == "when"
            push_and_tabulate([
                :script, @dont_indent_next_line, @dont_tab_up_next_text,
                !was_opened])
          end
        elsif block || text =~ /^-\s*(case|if)\b/
          push_and_tabulate([:script, @dont_indent_next_line, @dont_tab_up_next_text])
        end
      when FILTER; start_filtered(text[1..-1].downcase)
      when DOCTYPE
        return render_doctype(text) if text[0...3] == '!!!'
        return push_plain(text[3..-1].strip, :escape_html => false) if text[1..2] == "=="
        return push_script(text[2..-1].strip, :escape_html => false) if text[1] == SCRIPT
        return push_flat_script(text[2..-1].strip, :escape_html => false) if text[1] == FLAT_SCRIPT
        return push_plain(text[1..-1].strip, :escape_html => false) if text[1] == ?\s
        push_plain text
      when ESCAPE; push_plain text[1..-1]
      else push_plain text
      end
    end

    # If the text is a silent script text with one of Ruby's mid-block keywords,
    # returns the name of that keyword.
    # Otherwise, returns nil.
    def mid_block_keyword?(text)
      text[MID_BLOCK_KEYWORD_REGEX, 1]
    end

    # Evaluates `text` in the context of the scope object, but
    # does not output the result.
    def push_silent(text, can_suppress = false)
      flush_merged_text
      return if can_suppress && options[:suppress_eval]
      @precompiled << "#{text};"
    end

    # Adds `text` to `@buffer` with appropriate tabulation
    # without parsing it.
    def push_merged_text(text, tab_change = 0, indent = true)
      text = !indent || @dont_indent_next_line || @options[:ugly] ? text : "#{'  ' * @output_tabs}#{text}"
      @to_merge << [:text, text, tab_change]
      @dont_indent_next_line = false
    end

    # Concatenate `text` to `@buffer` without tabulation.
    def concat_merged_text(text)
      @to_merge << [:text, text, 0]
    end

    def push_text(text, tab_change = 0)
      push_merged_text("#{text}\n", tab_change)
    end

    def flush_merged_text
      return if @to_merge.empty?

      str = ""
      mtabs = 0
      newlines = 0
      @to_merge.each do |type, val, tabs|
        case type
        when :text
          str << inspect_obj(val)[1...-1]
          mtabs += tabs
        when :script
          if mtabs != 0 && !@options[:ugly]
            val = "_hamlout.adjust_tabs(#{mtabs}); " + val
          end
          str << "\#{#{"\n" * newlines}#{val}}"
          mtabs = 0
          newlines = 0
        when :newlines
          newlines += val
        else
          raise SyntaxError.new("[HAML BUG] Undefined entry in Haml::Precompiler@to_merge.")
        end
      end

      unless str.empty?
        @precompiled <<
          if @options[:ugly]
            "_hamlout.buffer << \"#{str}\";"
          else
            "_hamlout.push_text(\"#{str}\", #{mtabs}, #{@dont_tab_up_next_text.inspect});"
          end
      end
      @precompiled << "\n" * newlines
      @to_merge = []
      @dont_tab_up_next_text = false
    end

    # Renders a block of text as plain text.
    # Also checks for an illegally opened block.
    def push_plain(text, options = {})
      if block_opened?
        raise SyntaxError.new("Illegal nesting: nesting within plain text is illegal.", @next_line.index)
      end

      if contains_interpolation?(text)
        options[:escape_html] = self.options[:escape_html] if options[:escape_html].nil?
        push_script(
          unescape_interpolation(text, :escape_html => options[:escape_html]),
          :escape_html => false)
      else
        push_text text
      end
    end

    # Adds +text+ to `@buffer` while flattening text.
    def push_flat(line)
      text = line.full.dup
      text = "" unless text.gsub!(/^#{@flat_spaces}/, '')
      @filter_buffer << "#{text}\n"
    end

    # Causes `text` to be evaluated in the context of
    # the scope object and the result to be added to `@buffer`.
    #
    # If `opts[:preserve_script]` is true, Haml::Helpers#find_and_flatten is run on
    # the result before it is added to `@buffer`
    def push_script(text, opts = {})
      raise SyntaxError.new("There's no Ruby code for = to evaluate.") if text.empty?
      text = handle_ruby_multiline(text)
      return if options[:suppress_eval]
      opts[:escape_html] = options[:escape_html] if opts[:escape_html].nil?

      args = %w[preserve_script in_tag preserve_tag escape_html nuke_inner_whitespace]
      args.map! {|name| opts[name.to_sym]}
      args << !block_opened? << @options[:ugly]

      no_format = @options[:ugly] &&
        !(opts[:preserve_script] || opts[:preserve_tag] || opts[:escape_html])
      output_expr = "(#{text}\n)"
      static_method = "_hamlout.#{static_method_name(:format_script, *args)}"

      # Prerender tabulation unless we're in a tag
      push_merged_text '' unless opts[:in_tag]

      unless block_opened?
        @to_merge << [:script, no_format ? "#{text}\n" : "#{static_method}(#{output_expr});"]
        concat_merged_text("\n") unless opts[:in_tag] || opts[:nuke_inner_whitespace]
        @newlines -= 1
        return
      end

      flush_merged_text

      push_silent "haml_temp = #{text}"
      newline_now
      push_and_tabulate([:loud, "_hamlout.buffer << #{no_format ? "haml_temp.to_s;" : "#{static_method}(haml_temp);"}",
        !(opts[:in_tag] || opts[:nuke_inner_whitespace] || @options[:ugly])])
    end

    # Causes `text` to be evaluated, and Haml::Helpers#find_and_flatten
    # to be run on it afterwards.
    def push_flat_script(text, options = {})
      flush_merged_text

      raise SyntaxError.new("There's no Ruby code for ~ to evaluate.") if text.empty?
      push_script(text, options.merge(:preserve_script => true))
    end

    def start_haml_comment
      return unless block_opened?

      @haml_comment = true
      push_and_tabulate([:haml_comment])
    end

    # Closes the most recent item in `@to_close_stack`.
    def close
      tag, *rest = @to_close_stack.pop
      send("close_#{tag}", *rest)
    end

    # Puts a line in `@precompiled` that will add the closing tag of
    # the most recently opened tag.
    def close_element(value)
      tag, nuke_outer_whitespace, nuke_inner_whitespace = value
      @output_tabs -= 1 unless nuke_inner_whitespace
      @template_tabs -= 1
      rstrip_buffer! if nuke_inner_whitespace
      push_merged_text("</#{tag}>" + (nuke_outer_whitespace ? "" : "\n"),
                       nuke_inner_whitespace ? 0 : -1, !nuke_inner_whitespace)
      @dont_indent_next_line = nuke_outer_whitespace
    end

    # Closes a Ruby block.
    def close_script(_1, _2, push_end = true)
      push_silent("end", true) if push_end
      @template_tabs -= 1
    end

    # Closes a comment.
    def close_comment(has_conditional)
      @output_tabs -= 1
      @template_tabs -= 1
      close_tag = has_conditional ? "<![endif]-->" : "-->"
      push_text(close_tag, -1)
    end

    # Closes a loud Ruby block.
    def close_loud(command, add_newline, push_end = true)
      push_silent('end', true) if push_end
      @precompiled << command
      @template_tabs -= 1
      concat_merged_text("\n") if add_newline
    end

    # Closes a filtered block.
    def close_filtered(filter)
      filter.internal_compile(self, @filter_buffer)
      @flat = false
      @flat_spaces = nil
      @filter_buffer = nil
      @template_tabs -= 1
    end

    def close_haml_comment
      @haml_comment = false
      @template_tabs -= 1
    end

    def close_nil(*args)
      @template_tabs -= 1
    end

    # This is a class method so it can be accessed from {Haml::Helpers}.
    #
    # Iterates through the classes and ids supplied through `.`
    # and `#` syntax, and returns a hash with them as attributes,
    # that can then be merged with another attributes hash.
    def self.parse_class_and_id(list)
      attributes = {}
      list.scan(/([#.])([-:_a-zA-Z0-9]+)/) do |type, property|
        case type
        when '.'
          if attributes['class']
            attributes['class'] += " "
          else
            attributes['class'] = ""
          end
          attributes['class'] += property
        when '#'; attributes['id'] = property
        end
      end
      attributes
    end

    def parse_static_hash(text)
      attributes = {}
      scanner = StringScanner.new(text)
      scanner.scan(/\s+/)
      until scanner.eos?
        return unless key = scanner.scan(LITERAL_VALUE_REGEX)
        return unless scanner.scan(/\s*=>\s*/)
        return unless value = scanner.scan(LITERAL_VALUE_REGEX)
        return unless scanner.scan(/\s*(?:,|$)\s*/)
        attributes[eval(key).to_s] = eval(value).to_s
      end
      text.count("\n").times { newline }
      attributes
    end

    # This is a class method so it can be accessed from Buffer.
    def self.build_attributes(is_html, attr_wrapper, attributes = {})
      quote_escape = attr_wrapper == '"' ? "&quot;" : "&apos;"
      other_quote_char = attr_wrapper == '"' ? "'" : '"'

      if attributes['data'].is_a?(Hash)
        attributes = attributes.dup
        attributes =
          Haml::Util.map_keys(attributes.delete('data')) {|name| "data-#{name}"}.merge(attributes)
      end

      result = attributes.collect do |attr, value|
        next if value.nil?

        value = filter_and_join(value, ' ') if attr == 'class'
        value = filter_and_join(value, '_') if attr == 'id'

        if value == true
          next " #{attr}" if is_html
          next " #{attr}=#{attr_wrapper}#{attr}#{attr_wrapper}"
        elsif value == false
          next
        end

        value = Haml::Helpers.preserve(Haml::Helpers.escape_once(value.to_s))
        # We want to decide whether or not to escape quotes
        value.gsub!('&quot;', '"')
        this_attr_wrapper = attr_wrapper
        if value.include? attr_wrapper
          if value.include? other_quote_char
            value = value.gsub(attr_wrapper, quote_escape)
          else
            this_attr_wrapper = other_quote_char
          end
        end
        " #{attr}=#{this_attr_wrapper}#{value}#{this_attr_wrapper}"
      end
      result.compact.sort.join
    end

    def self.filter_and_join(value, separator)
      return "" if value == ""
      value = [value] unless value.is_a?(Array)
      value = value.flatten.collect {|item| item ? item.to_s : nil}.compact.join(separator)
      return !value.empty? && value
    end

    def prerender_tag(name, self_close, attributes)
      attributes_string = Precompiler.build_attributes(html?, @options[:attr_wrapper], attributes)
      "<#{name}#{attributes_string}#{self_close && xhtml? ? ' /' : ''}>"
    end

    # Parses a line into tag_name, attributes, attributes_hash, object_ref, action, value
    def parse_tag(line)
      raise SyntaxError.new("Invalid tag: \"#{line}\".") unless match = line.scan(/%([-:\w]+)([-:\w\.\#]*)(.*)/)[0]
      tag_name, attributes, rest = match
      new_attributes_hash = old_attributes_hash = last_line = object_ref = nil
      attributes_hashes = {}
      while rest
        case rest[0]
        when ?{
          break if old_attributes_hash
          old_attributes_hash, rest, last_line = parse_old_attributes(rest)
          attributes_hashes[:old] = old_attributes_hash
        when ?(
          break if new_attributes_hash
          new_attributes_hash, rest, last_line = parse_new_attributes(rest)
          attributes_hashes[:new] = new_attributes_hash
        when ?[
          break if object_ref
          object_ref, rest = balance(rest, ?[, ?])
        else; break
        end
      end

      if rest
        nuke_whitespace, action, value = rest.scan(/(<>|><|[><])?([=\/\~&!])?(.*)?/)[0]
        nuke_whitespace ||= ''
        nuke_outer_whitespace = nuke_whitespace.include? '>'
        nuke_inner_whitespace = nuke_whitespace.include? '<'
      end

      value = value.to_s.strip
      [tag_name, attributes, attributes_hashes, object_ref, nuke_outer_whitespace,
       nuke_inner_whitespace, action, value, last_line || @index]
    end

    def parse_old_attributes(line)
      line = line.dup
      last_line = @index

      begin
        attributes_hash, rest = balance(line, ?{, ?})
      rescue SyntaxError => e
        if line.strip[-1] == ?, && e.message == "Unbalanced brackets."
          line << "\n" << @next_line.text
          last_line += 1
          next_line
          retry
        end

        raise e
      end

      attributes_hash = attributes_hash[1...-1] if attributes_hash
      return attributes_hash, rest, last_line
    end

    def parse_new_attributes(line)
      line = line.dup
      scanner = StringScanner.new(line)
      last_line = @index
      attributes = {}

      scanner.scan(/\(\s*/)
      loop do
        name, value = parse_new_attribute(scanner)
        break if name.nil?

        if name == false
          text = (Haml::Shared.balance(line, ?(, ?)) || [line]).first
          raise Haml::SyntaxError.new("Invalid attribute list: #{text.inspect}.", last_line - 1)
        end
        attributes[name] = value
        scanner.scan(/\s*/)

        if scanner.eos?
          line << " " << @next_line.text
          last_line += 1
          next_line
          scanner.scan(/\s*/)
        end
      end

      static_attributes = {}
      dynamic_attributes = "{"
      attributes.each do |name, (type, val)|
        if type == :static
          static_attributes[name] = val
        else
          dynamic_attributes << inspect_obj(name) << " => " << val << ","
        end
      end
      dynamic_attributes << "}"
      dynamic_attributes = nil if dynamic_attributes == "{}"

      return [static_attributes, dynamic_attributes], scanner.rest, last_line
    end

    def parse_new_attribute(scanner)
      unless name = scanner.scan(/[-:\w]+/)
        return if scanner.scan(/\)/)
        return false
      end

      scanner.scan(/\s*/)
      return name, [:static, true] unless scanner.scan(/=/) #/end

      scanner.scan(/\s*/)
      unless quote = scanner.scan(/["']/)
        return false unless var = scanner.scan(/(@@?|\$)?\w+/)
        return name, [:dynamic, var]
      end

      re = /((?:\\.|\#(?!\{)|[^#{quote}\\#])*)(#{quote}|#\{)/
      content = []
      loop do
        return false unless scanner.scan(re)
        content << [:str, scanner[1].gsub(/\\(.)/, '\1')]
        break if scanner[2] == quote
        content << [:ruby, balance(scanner, ?{, ?}, 1).first[0...-1]]
      end

      return name, [:static, content.first[1]] if content.size == 1
      return name, [:dynamic,
        '"' + content.map {|(t, v)| t == :str ? inspect_obj(v)[1...-1] : "\#{#{v}}"}.join + '"']
    end

    # Parses a line that will render as an XHTML tag, and adds the code that will
    # render that tag to `@precompiled`.
    def render_tag(line)
      tag_name, attributes, attributes_hashes, object_ref, nuke_outer_whitespace,
        nuke_inner_whitespace, action, value, last_line = parse_tag(line)

      raise SyntaxError.new("Illegal element: classes and ids must have values.") if attributes =~ /[\.#](\.|#|\z)/

      # Get rid of whitespace outside of the tag if we need to
      rstrip_buffer! if nuke_outer_whitespace

      preserve_tag = options[:preserve].include?(tag_name)
      nuke_inner_whitespace ||= preserve_tag
      preserve_tag &&= !options[:ugly]

      escape_html = (action == '&' || (action != '!' && @options[:escape_html]))

      case action
      when '/'; self_closing = true
      when '~'; parse = preserve_script = true
      when '='
        parse = true
        if value[0] == ?=
          value = unescape_interpolation(value[1..-1].strip, :escape_html => escape_html)
          escape_html = false
        end
      when '&', '!'
        if value[0] == ?= || value[0] == ?~
          parse = true
          preserve_script = (value[0] == ?~)
          if value[1] == ?=
            value = unescape_interpolation(value[2..-1].strip, :escape_html => escape_html)
            escape_html = false
          else
            value = value[1..-1].strip
          end
        elsif contains_interpolation?(value)
          value = unescape_interpolation(value, :escape_html => escape_html)
          parse = true
          escape_html = false
        end
      else
        if contains_interpolation?(value)
          value = unescape_interpolation(value, :escape_html => escape_html)
          parse = true
          escape_html = false
        end
      end

      if parse && @options[:suppress_eval]
        parse = false
        value = ''
      end

      object_ref = "nil" if object_ref.nil? || @options[:suppress_eval]

      attributes = Precompiler.parse_class_and_id(attributes)
      attributes_list = []

      if attributes_hashes[:new]
        static_attributes, attributes_hash = attributes_hashes[:new]
        Buffer.merge_attrs(attributes, static_attributes) if static_attributes
        attributes_list << attributes_hash
      end

      if attributes_hashes[:old]
        static_attributes = parse_static_hash(attributes_hashes[:old])
        Buffer.merge_attrs(attributes, static_attributes) if static_attributes
        attributes_list << attributes_hashes[:old] unless static_attributes || @options[:suppress_eval]
      end

      attributes_list.compact!

      raise SyntaxError.new("Illegal nesting: nesting within a self-closing tag is illegal.", @next_line.index) if block_opened? && self_closing
      raise SyntaxError.new("There's no Ruby code for #{action} to evaluate.", last_line - 1) if parse && value.empty?
      raise SyntaxError.new("Self-closing tags can't have content.", last_line - 1) if self_closing && !value.empty?

      if block_opened? && !value.empty? && !is_ruby_multiline?(value)
        raise SyntaxError.new("Illegal nesting: content can't be both given on the same line as %#{tag_name} and nested within it.", @next_line.index)
      end

      self_closing ||= !!(!block_opened? && value.empty? && @options[:autoclose].any? {|t| t === tag_name})
      value = nil if value.empty? && (block_opened? || self_closing)

      dont_indent_next_line =
        (nuke_outer_whitespace && !block_opened?) ||
        (nuke_inner_whitespace && block_opened?)

      # Check if we can render the tag directly to text and not process it in the buffer
      if object_ref == "nil" && attributes_list.empty? && !preserve_script
        tag_closed = !block_opened? && !self_closing && !parse

        open_tag  = prerender_tag(tag_name, self_closing, attributes)
        if tag_closed
          open_tag << "#{value}</#{tag_name}>"
          open_tag << "\n" unless nuke_outer_whitespace
        else
          open_tag << "\n" unless parse || nuke_inner_whitespace || (self_closing && nuke_outer_whitespace)
        end

        push_merged_text(open_tag, tag_closed || self_closing || nuke_inner_whitespace ? 0 : 1,
                         !nuke_outer_whitespace)

        @dont_indent_next_line = dont_indent_next_line
        return if tag_closed
      else
        flush_merged_text
        content = parse ? 'nil' : inspect_obj(value)
        if attributes_list.empty?
          attributes_list = ''
        elsif attributes_list.size == 1
          attributes_list = ", #{attributes_list.first}"
        else
          attributes_list = ", (#{attributes_list.join(").merge(")})"
        end

        args = [tag_name, self_closing, !block_opened?, preserve_tag, escape_html,
                attributes, nuke_outer_whitespace, nuke_inner_whitespace
               ].map {|v| inspect_obj(v)}.join(', ')
        push_silent "_hamlout.open_tag(#{args}, #{object_ref}, #{content}#{attributes_list})"
        @dont_tab_up_next_text = @dont_indent_next_line = dont_indent_next_line
      end

      return if self_closing

      if value.nil?
        push_and_tabulate([:element, [tag_name, nuke_outer_whitespace, nuke_inner_whitespace]])
        @output_tabs += 1 unless nuke_inner_whitespace
        return
      end

      if parse
        push_script(value, :preserve_script => preserve_script, :in_tag => true,
          :preserve_tag => preserve_tag, :escape_html => escape_html,
          :nuke_inner_whitespace => nuke_inner_whitespace)
        concat_merged_text("</#{tag_name}>" + (nuke_outer_whitespace ? "" : "\n"))
      end
    end

    # Renders a line that creates an XHTML tag and has an implicit div because of
    # `.` or `#`.
    def render_div(line)
      render_tag('%div' + line)
    end

    # Renders an XHTML comment.
    def render_comment(line)
      conditional, line = balance(line, ?[, ?]) if line[0] == ?[
      line.strip!
      conditional << ">" if conditional

      if block_opened? && !line.empty?
        raise SyntaxError.new('Illegal nesting: nesting within a tag that already has content is illegal.', @next_line.index)
      end

      open = "<!--#{conditional}"

      # Render it statically if possible
      unless line.empty?
        return push_text("#{open} #{line} #{conditional ? "<![endif]-->" : "-->"}")
      end

      push_text(open, 1)
      @output_tabs += 1
      push_and_tabulate([:comment, !conditional.nil?])
      unless line.empty?
        push_text(line)
        close
      end
    end

    # Renders an XHTML doctype or XML shebang.
    def render_doctype(line)
      raise SyntaxError.new("Illegal nesting: nesting within a header command is illegal.", @next_line.index) if block_opened?
      doctype = text_for_doctype(line)
      push_text doctype if doctype
    end

    def text_for_doctype(text)
      text = text[3..-1].lstrip.downcase
      if text.index("xml") == 0
        return nil if html?
        wrapper = @options[:attr_wrapper]
        return "<?xml version=#{wrapper}1.0#{wrapper} encoding=#{wrapper}#{text.split(' ')[1] || "utf-8"}#{wrapper} ?>"
      end

      if html5?
        '<!DOCTYPE html>'
      else
        version, type = text.scan(DOCTYPE_REGEX)[0]

        if xhtml?
          if version == "1.1"
            '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">'
          elsif version == "5"
            '<!DOCTYPE html>'
          else
            case type
            when "strict";   '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'
            when "frameset"; '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">'
            when "mobile";   '<!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">'
            when "rdfa";     '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML+RDFa 1.0//EN" "http://www.w3.org/MarkUp/DTD/xhtml-rdfa-1.dtd">'
            when "basic";    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd">'
            else             '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">'
            end
          end

        elsif html4?
          case type
          when "strict";   '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">'
          when "frameset"; '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">'
          else             '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'
          end
        end
      end
    end

    # Starts a filtered block.
    def start_filtered(name)
      raise Error.new("Invalid filter name \":#{name}\".") unless name =~ /^\w+$/
      raise Error.new("Filter \"#{name}\" is not defined.") unless filter = Filters.defined[name]

      push_and_tabulate([:filtered, filter])
      @flat = true
      @filter_buffer = String.new

      # If we don't know the indentation by now, it'll be set in Line#tabs
      @flat_spaces = @indentation * @template_tabs if @indentation
    end

    def raw_next_line
      text = @template.shift
      return unless text

      index = @template_index
      @template_index += 1

      return text, index
    end

    def next_line
      text, index = raw_next_line
      return unless text

      # :eod is a special end-of-document marker
      line =
        if text == :eod
          Line.new '-#', '-#', '-#', index, self, true
        else
          Line.new text.strip, text.lstrip.chomp, text, index, self, false
        end

      # `flat?' here is a little outdated,
      # so we have to manually check if either the previous or current line
      # closes the flat block,
      # as well as whether a new block is opened
      @line.tabs if @line
      unless (flat? && !closes_flat?(line) && !closes_flat?(@line)) ||
          (@line && @line.text[0] == ?: && line.full =~ %r[^#{@line.full[/^\s+/]}\s])
        if line.text.empty?
          newline
          return next_line
        end

        handle_multiline(line)
      end

      @next_line = line
    end

    def closes_flat?(line)
      line && !line.text.empty? && line.full !~ /^#{@flat_spaces}/
    end

    def un_next_line(line)
      @template.unshift line
      @template_index -= 1
    end

    def handle_multiline(line)
      return unless is_multiline?(line.text)
      line.text.slice!(-1)
      while new_line = raw_next_line.first
        break if new_line == :eod
        newline and next if new_line.strip.empty?
        break unless is_multiline?(new_line.strip)
        line.text << new_line.strip[0...-1]
        newline
      end
      un_next_line new_line
      resolve_newlines
    end

    # Checks whether or not +line+ is in a multiline sequence.
    def is_multiline?(text)
      text && text.length > 1 && text[-1] == MULTILINE_CHAR_VALUE && text[-2] == ?\s
    end

    def handle_ruby_multiline(text)
      text = text.rstrip
      return text unless is_ruby_multiline?(text)
      un_next_line @next_line.full
      begin
        new_line = raw_next_line.first
        break if new_line == :eod
        newline and next if new_line.strip.empty?
        text << " " << new_line.strip
        newline
      end while is_ruby_multiline?(new_line.strip)
      next_line
      resolve_newlines
      text
    end

    def is_ruby_multiline?(text)
      text && text.length > 1 && text[-1] == ?, && text[-2] != ?? && text[-3..-2] != "?\\"
    end

    def contains_interpolation?(str)
      str.include?('#{')
    end

    def unescape_interpolation(str, opts = {})
      res = ''
      rest = Haml::Shared.handle_interpolation inspect_obj(str) do |scan|
        escapes = (scan[2].size - 1) / 2
        res << scan.matched[0...-3 - escapes]
        if escapes % 2 == 1
          res << '#{'
        else
          content = eval('"' + balance(scan, ?{, ?}, 1)[0][0...-1] + '"')
          content = "Haml::Helpers.html_escape((#{content}))" if opts[:escape_html]
          res << '#{' + content + "}"# Use eval to get rid of string escapes
        end
      end
      res + rest
    end

    def balance(*args)
      res = Haml::Shared.balance(*args)
      return res if res
      raise SyntaxError.new("Unbalanced brackets.")
    end

    def block_opened?
      !flat? && @next_line.tabs > @line.tabs
    end

    # Pushes value onto `@to_close_stack` and increases
    # `@template_tabs`.
    def push_and_tabulate(value)
      @to_close_stack.push(value)
      @template_tabs += 1
    end

    def flat?
      @flat
    end

    def newline
      @newlines += 1
    end

    def newline_now
      @precompiled << "\n"
      @newlines -= 1
    end

    def resolve_newlines
      return unless @newlines > 0
      @to_merge << [:newlines, @newlines]
      @newlines = 0
    end

    # Get rid of and whitespace at the end of the buffer
    # or the merged text
    def rstrip_buffer!(index = -1)
      last = @to_merge[index]
      if last.nil?
        push_silent("_hamlout.rstrip!", false)
        @dont_tab_up_next_text = true
        return
      end

      case last.first
      when :text
        last[1].rstrip!
        if last[1].empty?
          @to_merge.slice! index
          rstrip_buffer! index
        end
      when :script
        last[1].gsub!(/\(haml_temp, (.*?)\);$/, '(haml_temp.rstrip, \1);')
        rstrip_buffer! index - 1
      when :newlines
        rstrip_buffer! index - 1
      else
        raise SyntaxError.new("[HAML BUG] Undefined entry in Haml::Precompiler@to_merge.")
      end
    end
  end
end
