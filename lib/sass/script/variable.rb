module Sass
  module Script
    # A SassScript parse node representing a variable.
    class Variable < Node
      # The name of the variable.
      #
      # @return [String]
      attr_reader :name

      # @param name [String] See \{#name}
      def initialize(name)
        @name = name
      end

      # @return [String] A string representation of the variable
      def inspect
        "!#{name}"
      end

      # Evaluates the variable.
      #
      # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
      # @return [Literal] The SassScript object that is the value of the variable
      # @raise [Sass::SyntaxError] if the variable is undefined
      def perform(environment)
        (val = environment.var(name)) && (return val)
        raise SyntaxError.new("Undefined variable: \"!#{name}\".")
      end
    end
  end
end
