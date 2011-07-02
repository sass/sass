module Sass
  module Tree
    # A dynamic node representing returning from a function.
    #
    # @see Sass::Tree
    class ReturnNode < Node
      # The expression to return.
      # @type [Script::Node]
      attr_reader :expr

      # @param expr [Script::Node] The expression to return
      def initialize(expr)
        @expr = expr
        super()
      end

      # Returns sub nodes that are not tree children.
      def subnodes
        Array(expr)
      end
    end
  end
end
