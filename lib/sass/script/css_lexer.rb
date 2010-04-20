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
          return unless uri = scan(URI)
          return [:string, Script::String.new(uri)]
        end

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
