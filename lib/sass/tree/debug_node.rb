module Sass
  module Tree
    class DebugNode < Node
      def initialize(expr, options)
        @expr = expr
        super(options)
      end

      protected

      def _perform(environment)
        res = @expr.perform(environment)
        if filename
          puts "#{filename}:#{line} DEBUG: #{res}"
        else
          puts "Line #{line} DEBUG: #{res}"
        end
        []
      end
    end
  end
end
