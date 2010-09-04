module Sass
  # SassFile represents a file that will be parsed by the sass engine
  #
  #   `filename` - The full filename of the file. This should make it clear
  #                to a user where to find the file so they can edit it.
  #                If this file is on disk, this must be the absolute filename of the file.
  #   `syntax`   - What syntax is the file. One of :sass, :scss, or :css
  #   `contents` - The raw contents of the file as a string.
  #   `source`   - The importer that found the file. Might be null if it
  #                was not imported.
  class SassFile
    attr_accessor :filename, :syntax, :contents, :source
    def initialize(filename, syntax, contents, source)
      @filename, @syntax, @contents, @source = filename, syntax, contents, source
    end
    def self.new_from_filename(filename)
      raise "Unknown Extension" unless filename =~ /^.*\.(css|sass|scss)$/
      new(File.expand_path(filename), $1.to_sym, (File.read(filename) rescue nil), nil)
    end
  end

  # An Importer can be placed onto the Sass load path and it will
  # find files and return them as SassFile objects.
  #
  # Note: Sass does not require an extension to import a file. But a user
  # may provide an extension and this means that they are choosing a specific
  # syntax to import. All Importers must adhere to this policy. 
  class Importer

    def self.default_filesystem_class
      @default_filesystem_class || FilesystemImporter
    end
    def self.default_filesystem_class=(klass)
      @default_filesystem_class = klass
    end
    
    # @param name [String] The name of the file that is requested to be imported.
    # @param context [SassFile] The file doing the import. The Importer must not
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

  class FilesystemImporter < Importer
    def initialize(root)
      @root = root
    end

    def find(name, context = nil)
      full_filename = nil
      full_filename = detect_within(File.dirname(context.filename), name) if context
      full_filename ||= detect_within(@root, name)
      return unless full_filename && File.readable?(full_filename)
      dirname, basename, extension = split(full_filename)
      SassFile.new(File.expand_path(full_filename), extension.to_sym, File.read(full_filename), self)
    end

    def detect_within(dir, name)
      each_possible_filename(name) do |possible|
        if File.exists?(full_path = "#{dir}/#{possible}")
          return full_path
        end
      end
      nil
    end

    def to_s
      @root
    end

    def on_disk?
      true
    end
  end
end