module Sass
  module Script
    class CssLexer < Lexer
      def token
        important || super
      end

      def string(*args)
        return unless scan(STRING)
        str = (@scanner[1] || @scanner[2]).
          gsub(/\\([^0-9a-f])/, '\1').
          gsub(/\\([0-9a-f]{1,4})/, "\\\\\\1")
        [:string, Script::String.new(str, :string)]
      end

      def important
        return unless s = scan(IMPORTANT)
        [:raw, s]
      end
    end
  end
end
