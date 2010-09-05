module Sass
  # SassFile represents a Sass, SCSS, or CSS file,
  # along with some metadata necessary for rendering that file.
  class SassFile
    # The full filename of the file. This should make it clear
    # to a user where to find the file so they can edit it.
    # If this file is on disk, this must be the absolute filename of the file.
    #
    # @return [String]
    attr_accessor :filename

    # The file's syntax. One of `:sass`, `:scss`, or `:css`.
    # @return [Symbol]
    attr_accessor :syntax

    # The contents of the file.
    # @return [String]
    attr_accessor :contents

    # The importer that found the file. `nil` if the file wasn't imported.
    # @return [Sass::Importer, nil]
    attr_accessor :source

    # @param filename [String] See \{#filename}.
    # @param syntax [Symbol] See \{#syntax}.
    # @param contents [String] See \{#contents}.
    # @param source [Sass::Importer] See \{#source}.
    def initialize(filename, syntax, contents, source = nil)
      @filename, @syntax, @contents, @source = filename, syntax, contents, source
    end

    # Reads a SassFile from a file on disk, and populates the metadata appropriately.
    #
    # @param filename [String] The path to the Sass, SCSS, or CSS file.
    # @raise [Sass::SyntaxError] If `filename` isn't a Sass, SCSS, or CSS filename.
    def self.new_from_filename(filename)
      ext = File.extname(filename)
      raise Sass::SyntaxError.new("Filename #{filename} has no extension") unless ext
      ext = ext.downcase
      raise Sass::SyntaxError.new("Unknown extension: #{ext}") unless %w[sass scss css].include?(ext)
      new(File.expand_path(filename), ext.to_sym, (File.read(filename) rescue nil))
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
