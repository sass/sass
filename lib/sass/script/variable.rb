module Sass
  module Script
    # A SassScript parse node representing a variable.
    class Variable < Node
      # The name of the variable.
      #
      # @return [String]
      attr_reader :name

      # The underscored name of the variable.
      #
      # @return [String]
      attr_reader :underscored_name

      # @param name [String] See \{#name}
      def initialize(name)
        @name = name
        @underscored_name = name.gsub(/-/,"_")
        super()
      end

      # @return [String] A string representation of the variable
      def inspect(opts = {})
        return "!important" if name == "important"
        "$#{dasherize(name, opts)}"
      end
      alias_method :to_sass, :inspect

      # Returns an empty array.
      #
      # @return [Array<Node>] empty
      # @see Node#children
      def children
        []
      end

      # @see Node#deep_copy
      def deep_copy
        dup
      end

      protected

      # Evaluates the variable.
      #
      # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
      # @return [Literal] The SassScript object that is the value of the variable
      # @raise [Sass::SyntaxError] if the variable is undefined
      def _perform(environment)
        raise SyntaxError.new("Undefined variable: \"$#{name}\".") unless val = environment.var(name)
        if val.is_a?(Number)
          val = val.dup
          val.original = nil
        end
        return val
      end
    end
  end
end
