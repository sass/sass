module Sass
  module SCSS
    class ScriptParser < Sass::Script::Parser
      private

      def assert_done
        @lexer.unpeek!
      end
    end
  end
end
