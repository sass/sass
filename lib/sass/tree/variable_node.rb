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

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}$#{dasherize(@name, opts)}: #{@expr.to_sass(opts)}#{' !default' if @guarded}#{semi fmt}\n"
      end

      # Loads the new variable value into the environment.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        return [] if @guarded && !environment.var(@name).nil?
        val = @expr.perform(environment)
        if @expr.context == :equals && val.is_a?(Sass::Script::String)
          val = Sass::Script::String.new(val.value)
        end
        environment.set_var(@name, val)
        []
      end
    end
  end
end
