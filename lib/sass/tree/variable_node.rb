module Sass
  module Tree
    # A dynamic node representing a variable definition.
    #
    # @see Sass::Tree
    class VariableNode < Node
      # @param name [String] The name of the variable
      # @param expr [Script::Node] The parse tree for the initial variable value
      # @param guarded [Boolean] Whether this is a guarded variable assignment (`||=`)
      def initialize(name, expr, guarded)
        @name = name
        @expr = expr
        @guarded = guarded
        super()
      end

      protected

      # Loads the new variable value into the environment.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        if @guarded && environment.var(@name).nil?
          environment.set_var(@name, @expr.perform(environment))
        elsif !@guarded
          environment.set_var(@name, @expr.perform(environment))
        end

        []
      end
    end
  end
end
