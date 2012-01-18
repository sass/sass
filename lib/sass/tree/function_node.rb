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
      attr_accessor :args

      # @param name [String] The function name
      # @param args [Array<(Script::Node, Script::Node)>] The arguments for the function.
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end
    end
  end
end
