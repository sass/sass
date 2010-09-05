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
    # @return [Sass::Importers::Base, nil]
    attr_accessor :source

    # @param filename [String] See \{#filename}.
    # @param syntax [Symbol] See \{#syntax}.
    # @param contents [String] See \{#contents}.
    # @param source [Sass::Importers::Base] See \{#source}.
    def initialize(filename, syntax, contents, source = nil)
      @filename, @syntax, @contents, @source = filename, syntax, contents, source
    end

    # Reads a SassFile from a file on disk, and populates the metadata appropriately.
    #
    # @param filename [String] The path to the Sass, SCSS, or CSS file.
    # @raise [Sass::SyntaxError] If `filename` isn't a Sass, SCSS, or CSS filename.
    def self.new_from_filename(filename)
      ext = filename[/\.(.*?)$/]
      raise Sass::SyntaxError.new("Filename #{filename} has no extension") unless ext
      ext = ext.downcase
      raise Sass::SyntaxError.new("Unknown extension: #{ext}") unless %w[sass scss css].include?(ext)
      new(File.expand_path(filename), ext.to_sym, (File.read(filename) rescue nil))
    end
  end
end