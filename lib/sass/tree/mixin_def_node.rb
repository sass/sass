module Sass
  module Tree
    # A dynamic node representing a mixin definition.
    #
    # @see Sass::Tree
    class MixinDefNode < Node
      # @param name [String] The mixin name
      # @param args [Array<(String, Script::Node)>] The arguments for the mixin.
      #   Each element is a tuple containing the name of the argument
      #   and the parse tree for the default value of the argument
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end

      protected

      # Loads the mixin into the environment.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        environment.set_mixin(@name, Sass::Mixin.new(@name, @args, environment, children))
        []
      end
    end
  end
end
