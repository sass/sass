module Sass
  module Tree
    # A static node that wraps the {Sass::Tree} for an `@import`ed file.
    # It doesn't have a functional purpose other than to add the `@import`ed file
    # to the backtrace if an error occurs.
    class FileNode < Node
      # @param filename [String] The name of the imported file
      def initialize(filename)
        @filename = filename
        super()
      end

      # Computes the CSS for the imported file.
      #
      # @param args [Array] Ignored
      def to_s(*args)
        @to_s ||= (style == :compressed ? super().strip : super())
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end

      def invisible?; to_s.empty?; end

      protected

      # Parses the imported file
      # and runs the dynamic Sass for it.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
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
