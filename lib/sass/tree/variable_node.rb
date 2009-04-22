module Sass
  module Tree
    class VariableNode < Node
      def initialize(name, expr, guarded)
        @name = name
        @expr = expr
        @guarded = guarded
        super()
      end

      protected

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
