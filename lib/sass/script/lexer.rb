require 'strscan'

module Sass
  module Script
    class Lexer # :nodoc:
      OPERATORS = {
        '+' => :plus,
        '-' => :minus,
        '*' => :times,
        '/' => :div,
        '%' => :mod,
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
      }
      # We'll want to match longer names first
      # so that > and < don't clobber >= and <=
      OP_NAMES = OPERATORS.keys.sort_by {|o| -o.size}

      def initialize(str)
        @scanner = StringScanner.new(str)
      end

      def token
        return if done?

        if @tok
          @tok, tok = nil, @tok
          return tok
        end

        whitespace
        variable || string || number || color || bool || op || ident ||
          (raise SyntaxError.new("Syntax error in '#{@scanner.string}' at '#{@scanner.rest}'."))
      end

      def peek
        @tok ||= token
      end

      def done?
        whitespace
        @scanner.eos? && @tok.nil?
      end

      def rest
        @scanner.rest
      end

      private

      def whitespace
        @scanner.scan(/\s*/)
      end

      def variable
        return unless @scanner.scan(/!(\w+)/)
        [:const, @scanner[1]]
      end

      def ident
        return unless s = @scanner.scan(/(\\.|[^\s\\+\-*\/%(),=!])+/)
        [:ident, s.gsub(/\\(.)/, '\1')]
      end

      def string
        return unless @scanner.scan(/"((?:\\.|[^"\\])*)"/)
        [:string, Script::String.new(@scanner[1].gsub(/\\(.)/, '\1'))]
      end

      def number
        return unless @scanner.scan(/(-)?(?:(\d*\.\d+)|(\d+))([a-zA-Z%]+)?/)
        value = @scanner[2] ? @scanner[2].to_f : @scanner[3].to_i
        value = -value if @scanner[1]
        [:number, Script::Number.new(value, Array(@scanner[4]))]
      end

      COLOR = /\##{"([0-9a-fA-F]{1,2})" * 3}|(#{Color::HTML4_COLORS.keys.join("|")})/
      def color
        return unless @scanner.scan(COLOR)
        value = if @scanner[4]
                  color = Color::HTML4_COLORS[@scanner[4].downcase]
                  (0..2).map {|n| color >> (n << 3) & 0xff}.reverse
                else
                  (1..3).map {|i| @scanner[i]}.map {|num| num.ljust(2, num).to_i(16)}
                end
        [:color, Script::Color.new(value)]
      end

      def bool
        return unless s = @scanner.scan(/(true|false)\b/)
        [:bool, Script::Bool.new(s == 'true')]
      end

      def op
        return unless op = OP_NAMES.detect do |s|
          @scanner.scan(Regexp.new(Regexp.escape(s) + (s =~ /\w$/ ? '(\b|$)' : '')))
        end
        [OPERATORS[op]]
      end
    end
  end
end
