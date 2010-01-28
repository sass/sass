module Sass
  module Tree
    # A dynamic node representing a Sass `@debug` statement.
    #
    # @see Sass::Tree
    class DebugNode < Node
      # @param expr [Script::Node] The expression to print
      def initialize(expr)
        @expr = expr
        super()
      end

      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}@debug #{@expr.to_sass}\n"
      end

      protected

      # Prints the expression to STDERR.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        res = @expr.perform(environment)
        if filename
          $stderr.puts "#{filename}:#{line} DEBUG: #{res}"
        else
          $stderr.puts "Line #{line} DEBUG: #{res}"
        end
        []
      end
    end
  end
end
