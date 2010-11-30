module Sass
  module Tree
    # A dynamic node representing returning from a fnuction.
    #
    # @see Sass::Tree
    class ReturnNode < Node
      # @param expr [Script::Node] The expression to return
      def initialize(expr)
        @expr = expr
        super()
      end

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}@return #{@expr.to_sass(opts)}#{semi fmt}\n"
      end

      # Returns the value of the expression.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        throw :_sass_return, @expr.perform(environment)
      end
    end
  end
end
