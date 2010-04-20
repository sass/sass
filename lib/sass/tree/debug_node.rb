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

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}@debug #{@expr.to_sass(opts)}#{semi fmt}\n"
      end

      # Prints the expression to STDERR.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        res = @expr.perform(environment)
        res = res.value if res.is_a?(Sass::Script::String)
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
