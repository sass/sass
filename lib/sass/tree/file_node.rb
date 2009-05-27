module Sass
  module Tree
    class FileNode < Node
      def initialize(filename)
        @filename = filename
        super()
      end

      def to_s(*args)
        @to_s ||= super()
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end

      def invisible?; to_s.empty?; end

      protected

      def perform!(environment)
        self.children = Sass::Files.tree_for(filename, @options).children
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end
    end
  end
end
