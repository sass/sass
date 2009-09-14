module Sass
  module Tree
    # A static node that wraps the {Sass::Tree} for an `@import`ed file.
    # It doesn't have a functional purpose other than to add the `@import`ed file
    # to the backtrace if an error occurs.
    class ImportNode < RootNode
      # @param imported_filename [String] The name of the imported file
      def initialize(imported_filename)
        @imported_filename = imported_filename
        super(nil)
      end

      def invisible?; to_s.empty?; end

      protected

      # Computes the CSS for the imported file.
      #
      # @param args [Array] Ignored
      def _to_s(*args)
        @to_s ||= (style == :compressed ? super.strip : super)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => children.first.filename)
        e.add_backtrace(:filename => @filename, :line => @line)
        raise e
      end

      # Parses the imported file
      # and runs the dynamic Sass for it.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def perform!(environment)
        return unless full_filename = import
        root = Sass::Files.tree_for(full_filename, @options)
        @template = root.template
        self.children = root.children
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => full_filename)
        e.add_backtrace(:filename => @filename, :line => @line)
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
          raise SyntaxError.new(e.message, :line => self.line, :filename => @filename)
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
