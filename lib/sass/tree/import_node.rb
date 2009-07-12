module Sass
  module Tree
    # A static node that wraps the {Sass::Tree} for an `@import`ed file.
    # It doesn't have a functional purpose other than to add the `@import`ed file
    # to the backtrace if an error occurs.
    class ImportNode < Node
      # @param imported_filename [String] The name of the imported file
      def initialize(imported_filename)
        @imported_filename = imported_filename
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
        return unless full_filename = import
        self.children = Sass::Files.tree_for(full_filename, @options).children
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.add_backtrace_entry(@filename)
        raise e
      end

      private

      def import_paths
        paths = (@options[:load_paths] || []).dup
        paths.unshift(File.dirname(@options[:filename])) if @options[:filename]
        paths
      end

      def import
        begin
          full_filename = Sass::Files.find_file_to_import(@imported_filename, import_paths)
        rescue Exception => e
          raise SyntaxError.new(e.message, self.line)
        end

        if full_filename =~ /\.css$/
          @to_s = "@import url(#{full_filename});"
          return false
        end

        return full_filename
      end
    end
  end
end
