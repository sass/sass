module Sass
  module Tree
    # A dynamic node representing a Sass `@debug` statement.
    #
    # @see Sass::Tree
    class DebugNode < Node
      # The expression to print.
      # @return [Script::Node] 
      attr_reader :expr

      # @param expr [Script::Node] The expression to print
      def initialize(expr)
        @expr = expr
        super()
      end
    end
  end
end
