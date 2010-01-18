require 'sass/scss/rx'

require 'strscan'

module Sass
  module Script
    # The lexical analyzer for SassScript.
    # It takes a raw string and converts it to individual tokens
    # that are easier to parse.
    class Lexer
      # A struct containing information about an individual token.
      #
      # `type`: \[`Symbol`\]
      # : The type of token.
      #
      # `value`: \[`Object`\]
      # : The Ruby object corresponding to the value of the token.
      #
      # `line`: \[`Fixnum`\]
      # : The line of the source file on which the token appears.
      #
      # `offset`: \[`Fixnum`\]
      # : The number of bytes into the line the SassScript token appeared.
      Token = Struct.new(:type, :value, :line, :offset)

      # The line number of the lexer's current position.
      #
      # @return [Fixnum]
      attr_reader :line

      # The number of bytes into the current line
      # of the lexer's current position.
      #
      # @return [Fixnum]
      attr_reader :offset

      # A hash from operator strings to the corresponding token types.
      # @private
      OPERATORS = {
        '+' => :plus,
        '-' => :minus,
        '*' => :times,
        '/' => :div,
        '%' => :mod,
        '=' => :single_eq,
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
      }

      # A list of operator strings ordered with longer names first
      # so that `>` and `<` don't clobber `>=` and `<=`.
      # @private
      OP_NAMES = OPERATORS.keys.sort_by {|o| -o.size}

      # A hash of regular expressions that are used for tokenizing.
      # @private
      REGULAR_EXPRESSIONS = {
        :whitespace => /\s+/,
        :comment => Sass::SCSS::RX::COMMENT,
        :single_line_comment => Sass::SCSS::RX::SINGLE_LINE_COMMENT,
        :variable => /!(#{Sass::SCSS::RX::IDENT})/,
        :ident => Sass::SCSS::RX::IDENT,
        :number => /(-)?(?:(\d*\.\d+)|(\d+))([a-zA-Z%]+)?/,
        :color => Sass::SCSS::RX::HEXCOLOR,
        :bool => /(true|false)\b/,
        :op => %r{(#{Regexp.union(*OP_NAMES.map{|s| Regexp.new(Regexp.escape(s) + (s =~ /\w$/ ? '(?:\b|$)' : ''))})})}
      }

      class << self
        private
        def string_re(open, close)
          /#{open}((?:\\.|\#(?!\{)|[^#{close}\\#])*)(#{close}|(?=#\{))/
        end
      end

      # A hash of regular expressions that are used for tokenizing strings.
      #
      # The key is a [Symbol, Boolean] pair.
      # The symbol represents which style of quotation to use,
      # while the boolean represents whether or not the string
      # is following an interpolated segment.
      STRING_REGULAR_EXPRESSIONS = {
        [:double, false] => string_re('"', '"'),
        [:single, false] => string_re("'", "'"),
        [:double, true] => string_re('', '"'),
        [:single, true] => string_re('', "'"),
      }

      # @param str [String, StringScanner] The source text to lex
      # @param line [Fixnum] The line on which the SassScript appears.
      #   Used for error reporting
      # @param offset [Fixnum] The number of characters in on which the SassScript appears.
      #   Used for error reporting
      # @param options [{Symbol => Object}] An options hash;
      #   see {file:SASS_REFERENCE.md#sass_options the Sass options documentation}
      def initialize(str, line, offset, options)
        @scanner = str.is_a?(StringScanner) ? str : StringScanner.new(str)
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
        return tok
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
        @scanner.pos -= @scanner.matched_size if @tok
      end

      # @return [Boolean] Whether or not there's more source text to lex.
      def done?
        whitespace unless after_interpolation?
        @scanner.eos? && @tok.nil?
      end

      private

      def read_token
        return if done?

        value = token
        unless value
          raise SyntaxError.new("Syntax error in '#{@scanner.string}' at character #{current_position}.")
        end
        Token.new(value.first, value.last, @line, last_match_position)
      end

      def whitespace
        nil while scan(REGULAR_EXPRESSIONS[:whitespace]) ||
          scan(REGULAR_EXPRESSIONS[:comment]) ||
          scan(REGULAR_EXPRESSIONS[:single_line_comment])
      end

      def token
        return string(@interpolation_stack.pop, true) if after_interpolation?
        variable || string(:double, false) || string(:single, false) || number || color || bool || op || ident
      end

      def variable
        return unless scan(REGULAR_EXPRESSIONS[:variable])
        [:const, @scanner[1]]
      end

      def ident
        return unless s = scan(REGULAR_EXPRESSIONS[:ident])
        [:ident, s.gsub(/\\(.)/, '\1')]
      end

      def string(re, open)
        return unless scan(STRING_REGULAR_EXPRESSIONS[[re, open]])
        @interpolation_stack << re if @scanner[2].empty? # Started an interpolated section
        [:string, Script::String.new(@scanner[1].gsub(/\\([^0-9a-f])/, '\1').gsub(/\\([0-9a-f]{1,4})/, "\\\\\\1"))]
      end

      def number
        return unless scan(REGULAR_EXPRESSIONS[:number])
        value = @scanner[2] ? @scanner[2].to_f : @scanner[3].to_i
        value = -value if @scanner[1]
        [:number, Script::Number.new(value, Array(@scanner[4]))]
      end

      def color
        return unless s = scan(REGULAR_EXPRESSIONS[:color])
        value = s.scan(/^#(..?)(..?)(..?)$/).first.
          map {|num| num.ljust(2, num).to_i(16)}
        [:color, Script::Color.new(value)]
      end

      def bool
        return unless s = scan(REGULAR_EXPRESSIONS[:bool])
        [:bool, Script::Bool.new(s == 'true')]
      end

      def op
        prev_chr = @scanner.string[@scanner.pos - 1].chr
        return unless op = scan(REGULAR_EXPRESSIONS[:op])
        [OPERATORS[op]]
      end

      def scan(re)
        return unless str = @scanner.scan(re)
        c = str.count("\n")
        @line += str.count("\n")
        @offset = (c == 0 ? @offset + str.size : str[/\n(.*)/, 1].size)
        str
      end

      def current_position
        @offset + 1
      end

      def last_match_position
        current_position - @scanner.matched_size
      end

      def after_interpolation?
        @prev && @prev.type == :end_interpolation
      end
    end
  end
end
