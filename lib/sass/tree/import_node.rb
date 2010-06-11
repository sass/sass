module Sass
  module Tree
    # A static node that wraps the {Sass::Tree} for an `@import`ed file.
    # It doesn't have a functional purpose other than to add the `@import`ed file
    # to the backtrace if an error occurs.
    class ImportNode < RootNode
      # The name of the imported file as it appears in the Sass document.
      #
      # @return [String]
      attr_reader :imported_filename

      # @param imported_filename [String] The name of the imported file
      def initialize(imported_filename)
        @imported_filename = imported_filename
        super(nil)
      end

      def invisible?; to_s.empty?; end

      # Returns the resolved name of the imported file,
      # as returned by \{Sass::Files#find\_file\_to\_import}.
      #
      # @return [String] The filename of the imported file.
      #   This is an absolute path if the file is a `".sass"` or `".scss"` file.
      # @raise [Sass::SyntaxError] if `filename` ends in `".sass"` or `".scss"`
      #   and no corresponding Sass file could be found.
      def full_filename
        @full_filename ||= import
      end

      # @see Node#to_sass
      def to_sass(tabs = 0, opts = {})
        "#{'  ' * tabs}@import #{@imported_filename}\n"
      end

      # @see Node#to_scss
      def to_scss(tabs = 0, opts = {})
        "#{'  ' * tabs}@import \"#{@imported_filename}\";\n"
      end

      # @see Node#cssize
      def cssize(*args)
        super.first
      end

      protected

      # @see Node#_cssize
      def _cssize(*args)
        super.children
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => children.first.filename)
        e.add_backtrace(:filename => @filename, :line => @line)
        raise e
      end

      # Returns a static DirectiveNode if this is importing a CSS file,
      # or parses and includes the imported Sass file.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        return DirectiveNode.new("@import url(#{full_filename})") if full_filename =~ /\.css$/
        super
      end

      # Parses the imported file and runs the dynamic Sass for it.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def perform!(environment)
        environment.push_frame(:filename => @filename, :line => @line)
        options = @options.dup
        options.delete(:syntax)
        root = Sass::Files.tree_for(full_filename, options)
        @template = root.template
        self.children = root.children
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => full_filename)
        e.add_backtrace(:filename => @filename, :line => @line)
        raise e
      ensure
        environment.pop_frame
      end

      private

      def import_paths
        paths = (@options[:load_paths] || []).dup
        paths.unshift(File.dirname(@options[:filename])) if @options[:filename]
        paths
      end

      def import
        Sass::Files.find_file_to_import(@imported_filename, import_paths)
      rescue Exception => e
        raise SyntaxError.new(e.message, :line => self.line, :filename => @filename)
      end
    end
  end
end
