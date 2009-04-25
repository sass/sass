module Sass
  module Tree
    # A static node that wraps the {Sass::Tree} for an `@import`ed file.
    # It doesn't have a functional purpose other than to add the `@import`ed file
    # to the backtrace if an error occurs.
    class FileNode < Node
      # @param filename [String] The name of the imported file
      # @param children [Array<Tree::Node>] The first-level child nodes
      #   of the imported file's parse tree
      # @param options [Hash<Symbol, Object>] An options hash;
      #   see [the Sass options documentation](../../Sass.html#sass_options)
      def initialize(filename, children, options)
        @filename = filename
        super(options)
        self.children = children
      end

      # Computes the CSS for the imported file.
      #
      # @param args [Array] Ignored
      def to_s(*args)
        super()
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end

      protected

      # Run the dynamic Sass for the imported file.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def perform!(environment)
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end
    end
  end
end
