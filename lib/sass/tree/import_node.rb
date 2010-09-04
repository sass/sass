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

      # Returns the resolved imported file.
      #
      # @return [SassFile] The imported file or nil if not found
      def imported_file
        @imported_file ||= import
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
        if path = css_import?
          return DirectiveNode.new("@import url(#{path})")
        end
        super
      end

      # Parses the imported file and runs the dynamic Sass for it.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def perform!(environment)
        environment.push_frame(:filename => @filename, :line => @line, :file => imported_file)
        options = @options.dup
        options.delete(:syntax)
        root = Sass::Files.tree_for(imported_file, options)
        @template = root.template
        self.children = root.children
        self.children = perform_children(environment)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => imported_file.filename)
        e.add_backtrace(:filename => @filename, :line => @line)
        raise e
      ensure
        environment.pop_frame
      end

      private

      def import_paths
        @import_paths ||= begin
          @options[:load_paths] ||= []
          @options[:load_paths].map! do |p|
            if p.is_a? String
              Importer.default_filesystem_class.new(p)
            else
              p
            end
          end
        end
      end

      def current_sass_file
        @current_sass_file ||= @options[:file]
        @current_sass_file ||= if @options[:filename]
          SassFile.new_from_filename(@options[:filename])
        end
      end

      # whether or not this import specified the file with an extension
      def explicit_import?
        File.extname(@imported_filename).size > 0
      end

      # whether or not this import should emit a CSS @import declaration
      def css_import?
        if @imported_filename =~ /\.css$/
          @imported_filename
        elsif imported_file.is_a?(String) && imported_file =~ /\.css$/
          imported_file
        end
      end

      def import
        sass_file = current_sass_file
        paths = import_paths.dup
        paths.unshift(sass_file.source) if sass_file && sass_file.source
        import_paths.each do |p|
          if f = p.find(@imported_filename, sass_file)
            return f
          end
        end
        if explicit_import?
          message = "File to import not found or unreadable: #{@imported_filename}.\n"
          if import_paths.size == 1
            message << "Load path: #{import_paths.first}"
          else
            message << "Load paths:\n  " << import_paths.join("\n  ")
          end
          raise SyntaxError.new(message)
        else
          Haml::Util.haml_warn <<END
WARNING: Neither #{@imported_filename}.sass nor .scss found. Using #{@imported_filename}.css instead.
This behavior is deprecated and will be removed in a future version.
If you really need #{@imported_filename}.css, import it explicitly.
END
          return @imported_filename + '.css'
        end
      rescue Exception => e
        raise SyntaxError.new(e.message, :line => self.line, :filename => @filename)
      end
    end
  end
end
