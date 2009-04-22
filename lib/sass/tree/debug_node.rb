module Sass
  module Tree
    class DebugNode < Node
      def initialize(expr)
        @expr = expr
        super()
      end

      protected

      def _perform(environment)
        res = @expr.perform(environment)
        if filename
          STDERR.puts "#{filename}:#{line} DEBUG: #{res}"
        else
          STDERR.puts "Line #{line} DEBUG: #{res}"
        end
        []
      end
    end
  end
end
