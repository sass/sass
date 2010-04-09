module Sass
  module Script
    class CssLexer < Lexer
      def token
        important || super
      end

      def string(*args)
        return unless scan(STRING)
        [:string, Script::String.new((@scanner[1] || @scanner[2]).gsub(/\\(['"])/, '\1'), :string)]
      end

      def important
        return unless s = scan(IMPORTANT)
        [:raw, s]
      end
    end
  end
end
