module Sass
  module SCSS
    # A mixin for subclasses of {Sass::Script::Lexer}
    # that makes them usable by {SCSS::Parser} to parse SassScript.
    # In particular, the lexer doesn't support `!` for a variable prefix.
    module ScriptLexer
      private

      def variable
        return [:raw, "!important"] if scan(Sass::SCSS::RX::IMPORTANT)
        _variable(Sass::SCSS::RX::VARIABLE)
      end
    end
  end
end
