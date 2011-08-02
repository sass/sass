module Sass
  module Tree
    # A dynamic node representing a mixin definition.
    #
    # @see Sass::Tree
    class MixinDefNode < Node
      # The mixin name.
      # @return [String]
      attr_reader :name

      # The arguments for the mixin.
      # Each element is a tuple containing the variable for argument
      # and the parse tree for the default value of the argument.
      #
      # @return [Array<(Script::Node, Script::Node)>]
      attr_accessor :args

      # @param name [String] The mixin name
      # @param args [Array<(Script::Node, Script::Node)>] See \{#args}
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end
    end
  end
end
