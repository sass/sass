module Sass
  module Tree
    # A dynamic node representing a function definition.
    #
    # @see Sass::Tree
    class FunctionNode < Node
      # The name of the function.
      # @return [String]
      attr_reader :name

      # The arguments to the function. Each element is a tuple
      # containing the variable for argument and the parse tree for
      # the default value of the argument
      #
      # @return [Array<Script::Node>]
      attr_reader :args

      # @param name [String] The function name
      # @param args [Array<(Script::Node, Script::Node)>] The arguments for the function.
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end

      protected

      # Returns an error message if the given child node is invalid,
      # and false otherwise.
      #
      # {FunctionNode}s may only contain variable declarations and SassScript directives
      # (e.g. `@if`, `@each`, `@warn`).
      # @param child [Tree::Node] A potential child node
      # @return [String] An error message if the child is invalid, or nil otherwise
      def invalid_child?(child)
        super ||
          unless [CommentNode, DebugNode, EachNode, ForNode, IfNode,
              ReturnNode, VariableNode, WarnNode, WhileNode].any? {|c| child.is_a?(c)}
            "Functions can only contain variable declarations and certain directives."
          end
      end
    end
  end
end
