module Sass
  module SCSS
    # A mixin for subclasses of {Sass::Script::Parser}
    # that makes them usable by {SCSS::Parser} to parse SassScript.
    # In particular, the parser won't raise an error
    # when there's more content in the lexer once lexing is done.
    # In addition, the parser doesn't support `!` for a variable prefix.
    module ScriptParser
      private

      # @private
      def lexer_class
        klass = Class.new(super)
        klass.send(:include, ScriptLexer)
        klass
      end

      # Instead of raising an error when the parser is done,
      # rewind the StringScanner so that it hasn't consumed the final token.
      def assert_done
        @lexer.unpeek!
      end
    end
  end
end
