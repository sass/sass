module Sass
  module Tree
    # A dynamic node representing a Sass `@warn` statement.
    #
    # @see Sass::Tree
    class WarnNode < Node
      # The expression to print.
      # @return [Script::Node]
      attr_reader :expr

      # @param expr [Script::Node] The expression to print
      def initialize(expr)
        @expr = expr
        super()
      end

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}@warn #{@expr.to_sass(opts)}#{semi fmt}\n"
      end
    end
  end
end
