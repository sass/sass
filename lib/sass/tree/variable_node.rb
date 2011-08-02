module Sass
  module Tree
    # A dynamic node representing a variable definition.
    #
    # @see Sass::Tree
    class VariableNode < Node
      # The name of the variable.
      # @return [String]
      attr_reader :name

      # The parse tree for the variable value.
      # @return [Script::Node]
      attr_reader :expr

      # Whether this is a guarded variable assignment (`!default`).
      # @return [Boolean]
      attr_reader :guarded

      # @param name [String] The name of the variable
      # @param expr [Script::Node] See \{#expr}
      # @param guarded [Boolean] See \{#guarded}
      def initialize(name, expr, guarded)
        @name = name
        @expr = expr
        @guarded = guarded
        super()
      end
    end
  end
end
