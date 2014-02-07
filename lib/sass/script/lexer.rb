require 'sass/scss/rx'

module Sass
  module Script
    # The lexical analyzer for SassScript.
    # It takes a raw string and converts it to individual tokens
    # that are easier to parse.
    class Lexer
      include Sass::SCSS::RX

      # A struct containing information about an individual token.
      #
      # `type`: \[`Symbol`\]
      # : The type of token.
      #
      # `value`: \[`Object`\]
      # : The Ruby object corresponding to the value of the token.
      #
      # `source_range`: \[`Sass::Source::Range`\]
      # : The range in the source file in which the token appeared.
      #
      # `pos`: \[`Fixnum`\]
      # : The scanner position at which the SassScript token appeared.
      Token = Struct.new(:type, :value, :source_range, :pos)

      # The line number of the lexer's current position.
      #
      # @return [Fixnum]
      def line
        return @line unless @tok
        @tok.source_range.start_pos.line
      end

      # The number of bytes into the current line
      # of the lexer's current position (1-based).
      #
      # @return [Fixnum]
      def offset
        return @offset unless @tok
        @tok.source_range.start_pos.offset
      end

      # A hash from operator strings to the corresponding token types.
      OPERATORS = {
        '+' => :plus,
        '-' => :minus,
        '*' => :times,
        '/' => :div,
        '%' => :mod,
        '=' => :single_eq,
        ':' => :colon,
        '(' => :lparen,
        ')' => :rparen,
        ',' => :comma,
        'and' => :and,
        'or' => :or,
        'not' => :not,
        '==' => :eq,
        '!=' => :neq,
        '>=' => :gte,
        '<=' => :lte,
        '>' => :gt,
        '<' => :lt,
        '#{' => :begin_interpolation,
        '}' => :end_interpolation,
        ';' => :semicolon,
        '{' => :lcurly,
        '...' => :splat,
      }

      OPERATORS_REVERSE = Sass::Util.map_hash(OPERATORS) {|k, v| [v, k]}

      TOKEN_NAMES = Sass::Util.map_hash(OPERATORS_REVERSE) {|k, v| [k, v.inspect]}.merge(
          :const => "variable (e.g. $foo)",
          :ident => "identifier (e.g. middle)")

      # A list of operator strings ordered with longer names first
      # so that `>` and `<` don't clobber `>=` and `<=`.
      OP_NAMES = OPERATORS.keys.sort_by {|o| -o.size}

      # A sub-list of {OP_NAMES} that only includes operators
      # with identifier names.
      IDENT_OP_NAMES = OP_NAMES.select {|k, v| k =~ /^\w+/}

      # A hash of regular expressions that are used for tokenizing.
      REGULAR_EXPRESSIONS = {
        :whitespace => /\s+/,
        :comment => COMMENT,
        :single_line_comment => SINGLE_LINE_COMMENT,
        :variable => /(\$)(#{IDENT})/,
        :ident => /(#{IDENT})(\()?/,
        :number => /(?:(\d*\.\d+)|(\d+))([a-zA-Z%]+)?/,
        :unary_minus_number => /-(?:(\d*\.\d+)|(\d+))([a-zA-Z%]+)?/,
        :color => HEXCOLOR,
        :ident_op => /(#{Regexp.union(*IDENT_OP_NAMES.map do |s|
          Regexp.new(Regexp.escape(s) + "(?!#{NMCHAR}|\Z)")
        end)})/,
        :op => /(#{Regexp.union(*OP_NAMES)})/,
      }

      class << self
        private

        def string_re(open, close)
          /#{open}((?:\\.|\#(?!\{)|[^#{close}\\#])*)(#{close}|#\{)/
        end
      end

      # A hash of regular expressions that are used for tokenizing strings.
      #
      # The key is a `[Symbol, Boolean]` pair.
      # The symbol represents which style of quotation to use,
      # while the boolean represents whether or not the string
      # is following an interpolated segment.
      STRING_REGULAR_EXPRESSIONS = {
        :double => {
          false => string_re('"', '"'),
          true => string_re('', '"')
        },
        :single => {
          false => string_re("'", "'"),
          true => string_re('', "'")
        },
        :uri => {
          false => /url\(#{W}(#{URLCHAR}*?)(#{W}\)|#\{)/,
          true => /(#{URLCHAR}*?)(#{W}\)|#\{)/
        },
        # Defined in https://developer.mozilla.org/en/CSS/@-moz-document as a
        # non-standard version of http://www.w3.org/TR/css3-conditional/
        :url_prefix => {
          false => /url-prefix\(#{W}(#{URLCHAR}*?)(#{W}\)|#\{)/,
          true => /(#{URLCHAR}*?)(#{W}\)|#\{)/
        },
        :domain => {
          false => /domain\(#{W}(#{URLCHAR}*?)(#{W}\)|#\{)/,
          true => /(#{URLCHAR}*?)(#{W}\)|#\{)/
        }
      }

      # @param str [String, StringScanner] The source text to lex
      # @param line [Fixnum] The 1-based line on which the SassScript appears.
      #   Used for error reporting and sourcemap building
      # @param offset [Fixnum] The 1-based character (not byte) offset in the line in the source.
      #   Used for error reporting and sourcemap building
      # @param options [{Symbol => Object}] An options hash;
      #   see {file:SASS_REFERENCE.md#sass_options the Sass options documentation}
      def initialize(str, line, offset, options)
        @scanner = str.is_a?(StringScanner) ? str : Sass::Util::MultibyteStringScanner.new(str)
        @line = line
        @offset = offset
        @options = options
        @interpolation_stack = []
        @prev = nil
      end

      # Moves the lexer forward one token.
      #
      # @return [Token] The token that was moved past
      def next
        @tok ||= read_token
        @tok, tok = nil, @tok
        @prev = tok
        tok
      end

      # Returns whether or not there's whitespace before the next token.
      #
      # @return [Boolean]
      def whitespace?(tok = @tok)
        if tok
          @scanner.string[0...tok.pos] =~ /\s\Z/
        else
          @scanner.string[@scanner.pos, 1] =~ /^\s/ ||
            @scanner.string[@scanner.pos - 1, 1] =~ /\s\Z/
        end
      end

      # Returns the next token without moving the lexer forward.
      #
      # @return [Token] The next token
      def peek
        @tok ||= read_token
      end

      # Rewinds the underlying StringScanner
      # to before the token returned by \{#peek}.
      def unpeek!
        if @tok
          @scanner.pos = @tok.pos
          @line = @tok.source_range.start_pos.line
          @offset = @tok.source_range.start_pos.offset
        end
      end

      # @return [Boolean] Whether or not there's more source text to lex.
      def done?
        whitespace unless after_interpolation? && @interpolation_stack.last
        @scanner.eos? && @tok.nil?
      end

      # @return [Boolean] Whether or not the last token lexed was `:end_interpolation`.
      def after_interpolation?
        @prev && @prev.type == :end_interpolation
      end

      # Raise an error to the effect that `name` was expected in the input stream
      # and wasn't found.
      #
      # This calls \{#unpeek!} to rewind the scanner to immediately after
      # the last returned token.
      #
      # @param name [String] The name of the entity that was expected but not found
      # @raise [Sass::SyntaxError]
      def expected!(name)
        unpeek!
        Sass::SCSS::Parser.expected(@scanner, name, @line)
      end

      # Records all non-comment text the lexer consumes within the block
      # and returns it as a string.
      #
      # @yield A block in which text is recorded
      # @return [String]
      def str
        old_pos = @tok ? @tok.pos : @scanner.pos
        yield
        new_pos = @tok ? @tok.pos : @scanner.pos
        @scanner.string[old_pos...new_pos]
      end

      private

      def read_token
        return if done?
        start_pos = source_position
        value = token
        return unless value
        type, val = value
        Token.new(type, val, range(start_pos), @scanner.pos - @scanner.matched_size)
      end

      def whitespace
        nil while scan(REGULAR_EXPRESSIONS[:whitespace]) ||
          scan(REGULAR_EXPRESSIONS[:comment]) ||
          scan(REGULAR_EXPRESSIONS[:single_line_comment])
      end

      def token
        if after_interpolation? && (interp_type = @interpolation_stack.pop)
          return string(interp_type, true)
        end

        variable || string(:double, false) || string(:single, false) || number || color ||
          string(:uri, false) || raw(UNICODERANGE) || special_fun || special_val || ident_op ||
          ident || op
      end

      def variable
        _variable(REGULAR_EXPRESSIONS[:variable])
      end

      def _variable(rx)
        return unless scan(rx)

        [:const, @scanner[2]]
      end

      def ident
        return unless scan(REGULAR_EXPRESSIONS[:ident])
        [@scanner[2] ? :funcall : :ident, @scanner[1]]
      end

      def string(re, open)
        return unless scan(STRING_REGULAR_EXPRESSIONS[re][open])
        if @scanner[2] == '#{' # '
          @scanner.pos -= 2 # Don't actually consume the #{
          @offset -= 2
          @interpolation_stack << re
        end
        str =
          if re == :uri
            url = "#{'url(' unless open}#{@scanner[1]}#{')' unless @scanner[2] == '#{'}"
            Script::Value::String.new(url)
          else
            Script::Value::String.new(@scanner[1].gsub(/\\(['"]|\#\{)/, '\1'), :string)
          end
        [:string, str]
      end

      def number
        # Handling unary minus is complicated by the fact that whitespace is an
        # operator in SassScript. We want "1-2" to be parsed as "1 - 2", but we
        # want "1 -2" to be parsed as "1 (-2)". To accomplish this, we only
        # parse a unary minus as part of a number literal if there's whitespace
        # before and not after it. Cases like "(-2)" are handled by the unary
        # minus logic in the parser instead.
        if @scanner.peek(1) == '-'
          return if @scanner.pos == 0
          @scanner.pos -= 1
          # Don't use @scanner.scan so we don't mess up the match data.
          unary_minus_allowed = @scanner.peek(1) =~ /\s/
          @scanner.pos += 1

          return unless unary_minus_allowed
          return unless scan(REGULAR_EXPRESSIONS[:unary_minus_number])
          minus = true
        else
          return unless scan(REGULAR_EXPRESSIONS[:number])
          minus = false
        end

        value = (@scanner[1] ? @scanner[1].to_f : @scanner[2].to_i) * (minus ? -1 : 1)
        script_number = Script::Value::Number.new(value, Array(@scanner[3]))
        [:number, script_number]
      end

      def color
        s = scan(REGULAR_EXPRESSIONS[:color])
        return unless s
        raise Sass::SyntaxError.new(<<MESSAGE.rstrip) unless s.size == 4 || s.size == 7
Colors must have either three or six digits: '#{s}'
MESSAGE
        script_color = Script::Value::Color.from_hex(s)
        [:color, script_color]
      end

      def special_fun
        str1 = scan(/((-[\w-]+-)?(calc|element)|expression|progid:[a-z\.]*)\(/i)
        return unless str1
        str2, _ = Sass::Shared.balance(@scanner, ?(, ?), 1)
        c = str2.count("\n")
        old_line = @line
        old_offset = @offset
        @line += c
        @offset = c == 0 ? @offset + str2.size : str2[/\n([^\n]*)/, 1].size + 1
        [:special_fun,
         Sass::Util.merge_adjacent_strings(
            [str1] + Sass::Engine.parse_interp(str2, old_line, old_offset, @options)),
         str1.size + str2.size]
      end

      def special_val
        return unless scan(/!important/i)
        [:string, Script::Value::String.new("!important")]
      end

      def ident_op
        op = scan(REGULAR_EXPRESSIONS[:ident_op])
        return unless op
        [OPERATORS[op]]
      end

      def op
        op = scan(REGULAR_EXPRESSIONS[:op])
        return unless op
        @interpolation_stack << nil if op == :begin_interpolation
        [OPERATORS[op]]
      end

      def raw(rx)
        val = scan(rx)
        return unless val
        [:raw, val]
      end

      def scan(re)
        str = @scanner.scan(re)
        return unless str
        c = str.count("\n")
        @line += c
        @offset = (c == 0 ? @offset + str.size : str.size - str.rindex("\n") + 1)
        str
      end

      def range(start_pos, end_pos = source_position)
        Sass::Source::Range.new(start_pos, end_pos, @options[:filename], @options[:importer])
      end

      def source_position
        Sass::Source::Position.new(@line, @offset)
      end
    end
  end
end
