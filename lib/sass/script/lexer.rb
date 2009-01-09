require 'strscan'

module Sass
  module Script
    class Lexer # :nodoc:
      Token = Struct.new(:type, :value, :line, :offset)

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
        '}' => :right_bracket,
      }

      # We'll want to match longer names first
      # so that > and < don't clobber >= and <=
      OP_NAMES = OPERATORS.keys.sort_by {|o| -o.size}

      REGULAR_EXPRESSIONS = {
        :whitespace => /\s*/,
        :variable => /!(\w+)/,
        :ident => /(\\.|[^\s\\+\-*\/%(),=!])+/,
        :string => /["}]((?:\\.|\#[^{]|[^"\\#])*)(?:"|#\{)/,
        :number => /(-)?(?:(\d*\.\d+)|(\d+))([a-zA-Z%]+)?/,
        :color => /\##{"([0-9a-fA-F]{1,2})" * 3}|(#{Color::HTML4_COLORS.keys.join("|")})/,
        :bool => /(true|false)\b/,
        :op => %r{(#{Regexp.union(*OP_NAMES.map{|s| Regexp.new(Regexp.escape(s) + (s =~ /\w$/ ? '(?:\b|$)' : ''))})})}
      }

      def initialize(str, line, offset)
        @scanner = str.is_a?(StringScanner) ? str : StringScanner.new(str)
        @line = line
        @offset = offset
        @to_emit = []
      end

      def token
        if @tok
          @tok, tok = nil, @tok
          return tok
        end

        return if done?

        value = @to_emit.shift || variable || string || number || color || bool || op || ident
        unless value
          raise SyntaxError.new("Syntax error in '#{@scanner.string}' at character #{current_position}.")
        end
        Token.new(value.first, value.last, @line, last_match_position)
      end

      def peek
        @tok ||= token
      end

      def done?
        whitespace
        @scanner.eos? && @tok.nil? && @to_emit.empty?
      end

      def rest
        @scanner.rest
      end

      private

      def emit(*value)
        @to_emit.push value
      end

      def whitespace
        @scanner.scan(REGULAR_EXPRESSIONS[:whitespace])
      end

      def variable
        return unless @scanner.scan(REGULAR_EXPRESSIONS[:variable])
        [:const, @scanner[1]]
      end

      def ident
        return unless s = @scanner.scan(REGULAR_EXPRESSIONS[:ident])
        [:ident, s.gsub(/\\(.)/, '\1')]
      end

      def string
        return unless @scanner.scan(REGULAR_EXPRESSIONS[:string])
        emit(:right_bracket) if @scanner.matched[0] == ?}
        emit(:string, Script::String.new(@scanner[1].gsub(/\\([^0-9a-f])/, '\1').gsub(/\\([0-9a-f]{1,4})/, "\\\\\\1")))
        emit(:begin_interpolation) if @scanner.matched[-2..-1] == '#{'
        @to_emit.shift
      end

      def number
        return unless @scanner.scan(REGULAR_EXPRESSIONS[:number])
        value = @scanner[2] ? @scanner[2].to_f : @scanner[3].to_i
        value = -value if @scanner[1]
        [:number, Script::Number.new(value, Array(@scanner[4]))]
      end

      def color
        return unless @scanner.scan(REGULAR_EXPRESSIONS[:color])
        value = if @scanner[4]
                  color = Color::HTML4_COLORS[@scanner[4].downcase]
                else
                  (1..3).map {|i| @scanner[i]}.map {|num| num.ljust(2, num).to_i(16)}
                end
        [:color, Script::Color.new(value)]
      end

      def bool
        return unless s = @scanner.scan(REGULAR_EXPRESSIONS[:bool])
        [:bool, Script::Bool.new(s == 'true')]
      end

      def op
        return unless op = @scanner.scan(REGULAR_EXPRESSIONS[:op])
        [OPERATORS[op]]
      end

      protected

      def current_position
        @offset + @scanner.pos + 1
      end
      def last_match_position
        current_position - @scanner.matchedsize
      end
    end
  end
end
