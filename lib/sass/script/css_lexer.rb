module Sass
  module Script
    # This is a subclass of {Lexer} for use in parsing plain CSS properties.
    #
    # @see Sass::SCSS::CssParser
    class CssLexer < Lexer
      private

      def token
        important || super
      end

      def string(re, *args)
        if re == :uri
          uri = scan(URI)
          return unless uri
          return [:string, Script::Value::String.new(uri)]
        end

        return unless scan(STRING)
        [:string, Script::Value::String.new((@scanner[1] || @scanner[2]).gsub(/\\(['"])/, '\1'), :string)]
      end

      def important
        s = scan(IMPORTANT)
        return unless s
        [:raw, s]
      end
    end
  end
end
