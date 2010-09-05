module Sass
  module Importers
    # An importer can be placed onto the Sass load path and it will
    # find files and return them as SassFile objects.
    #
    # Note: Sass does not require an extension to import a file. But a user
    # may provide an extension and this means that they are choosing a specific
    # syntax to import. All Importers must adhere to this policy. 
    class Base

      def self.default_filesystem_class
        @default_filesystem_class || Filesystem
      end
      def self.default_filesystem_class=(klass)
        @default_filesystem_class = klass
      end

      # @param name [String] The name of the file that is requested to be imported.
      # @param context [SassFile] The file doing the import. The importer must not
      #   depend on the contents nor the source being set on the SassFile.
      # @return [SassFile] if one is found.
      def find(name, context = nil)
        raise "Implement Me"
      end

      # This will be used to tell the user where Sass looked
      # for missing imports.
      def to_s
        raise "Implement Me"
      end

      # Returns whether the file was found on disk -- for duck typing purposes.
      def on_disk?
        false
      end

      protected
      def split(name)
        extension = nil
        dirname, basename = File.dirname(name), File.basename(name)
        if basename =~ /^(.*)\.(css|sass|scss)$/
          basename = $1
          extension = $2
        end
        [dirname, basename, extension]
      end

      def possible_filenames(name)
        filenames = []
        each_possible_filename(name) {|fn| filenames << fn}
        filenames
      end

      def each_possible_filename(name)
        dirname, basename, extension = split(name)
        basenames = ["#{basename}", "_#{basename}"]
        extensions = if extension
          [extension]
        else
          ["sass", "scss", "css"]
        end
        basenames.each do |bn|
          extensions.each do |ext|
            yield "#{dirname}/#{bn}.#{ext}"
          end
        end
      end
    end
  end
end

      
