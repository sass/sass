module Sass
  module SCSS
    # A subclass of {Sass::Script::Parser}
    # that can be used as a subcomponent of {SCSS::Parser}.
    # In particular, the parser doesn't raise an error
    # when there's more content in the lexer once lexing is done.
    class ScriptParser < Sass::Script::Parser
      private

      # Instead of raising an error when the parser is done,
      # rewind the StringScanner so that it hasn't consumed the final token.
      def assert_done
        @lexer.unpeek!
      end
    end
  end
end
