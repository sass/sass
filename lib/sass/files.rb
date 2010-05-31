require 'digest/sha1'
require 'pathname'
require 'fileutils'

module Sass
  # This module contains various bits of functionality
  # related to finding and caching Sass files.
  module Files
    extend self

    # Returns the {Sass::Tree} for the given file,
    # reading it from the Sass cache if possible.
    #
    # @param filename [String] The path to the Sass or SCSS file
    # @param options [{Symbol => Object}] The options hash.
    #   Only the {file:SASS_REFERENCE.md#cache-option `:cache_location`} option is used
    # @raise [Sass::SyntaxError] if there's an error in the document.
    #   The caller has responsibility for setting backtrace information, if necessary
    def tree_for(filename, options)
      default_options = Sass::Engine::DEFAULT_OPTIONS.dup
      default_options.delete(:syntax)
      options = default_options.merge!(options)
      text = File.read(filename)

      if options[:cache] || options[:read_cache]
        compiled_filename = sassc_filename(filename, options)
        sha = Digest::SHA1.hexdigest(text)

        if root = try_to_read_sassc(filename, compiled_filename, sha)
          root.options = options.merge(:filename => filename)
          return root
        end
      end

      options = options.merge(:filename => filename)
      if filename =~ /\.scss$/
        options = {:syntax => :scss}.merge(options)
      elsif filename =~ /\.sass$/
        options = {:syntax => :sass}.merge(options)
      end

      engine = Sass::Engine.new(text, options)

      root = engine.to_tree
      try_to_write_sassc(root, compiled_filename, sha, options) if options[:cache]
      root
    end

    # Find the full filename of a Sass, SCSS, or CSS file to import.
    # This follows Sass's import rules:
    # if the filename given ends in `".sass"`, `".scss"`, or `".css"`,
    # it will try to find that type of file;
    # otherwise, it will try to find the corresponding Sass/SCSS file
    # and fall back on CSS if it's not available.
    #
    # Any Sass/SCSS filename returned will correspond to
    # an actual file of the corresponding type on the filesystem.
    # CSS filenames, however, may not;
    # they're expected to be put through directly to the stylesheet
    # as CSS `@import` statements.
    #
    # @param filename [String] The filename to search for
    # @param load_paths [Array<String>] The set of filesystem paths
    #   to search for Sass/SCSS files.
    # @return [String] The filename of the imported file.
    #   This is an absolute path if the file is a `".sass"` or `".scss"` file.
    # @raise [Sass::SyntaxError] if `filename` ends in `".sass"` or `".scss"`
    #   and no corresponding Sass/SCSS file could be found.
    def find_file_to_import(filename, load_paths)
      was_sass = was_scss = false
      original_filename = filename

      if [".sass", ".scss"].include?(filename[-5..-1])
        was_sass = filename[-5..-1] == ".sass"
        was_scss = filename[-5..-1] == ".scss"
        filename = filename[0...-5]
      elsif filename[-4..-1] == ".css"
        return filename
      end

      new_filename = nil
      load_paths = load_paths.uniq
      load_paths.each do |load_path|
        new_filename ||= find_full_path("#{filename}.sass", load_path) unless was_scss
        new_filename ||= find_full_path("#{filename}.scss", load_path) unless was_sass
      end

      return new_filename if new_filename
      unless was_sass || was_scss
        Haml::Util.haml_warn <<END
WARNING: Neither #{filename}.sass nor .scss found. Using #{filename}.css instead.
This behavior is deprecated and will be removed in a future version.
If you really need #{filename}.css, import it explicitly.
END
        return filename + '.css'
      end

      message = "File to import not found or unreadable: #{original_filename}.\n"
      if load_paths.size == 1
        message << "Load path: #{load_paths.first}"
      else
        message << "Load paths:\n  " << load_paths.join("\n  ")
      end

      raise SyntaxError.new(message)
    end

    private

    def sassc_filename(filename, options)
      File.join(options[:cache_location],
        Digest::SHA1.hexdigest(File.dirname(File.expand_path(filename))),
        File.basename(filename) + 'c')
    end

    def try_to_read_sassc(filename, compiled_filename, sha)
      return unless File.readable?(compiled_filename)

      File.open(compiled_filename, "rb") do |f|
        return unless f.readline("\n").strip == Sass::VERSION
        return unless f.readline("\n").strip == sha
        return Marshal.load(f.read)
      end
    rescue EOFError, TypeError, ArgumentError => e
      Haml::Util.haml_warn "Warning. Error encountered while reading cache #{compiled_filename}: #{e}"
    end

    def try_to_write_sassc(root, compiled_filename, sha, options)
      return unless File.writable?(File.dirname(options[:cache_location]))
      return if File.exists?(options[:cache_location]) && !File.writable?(options[:cache_location])
      return if File.exists?(File.dirname(compiled_filename)) && !File.writable?(File.dirname(compiled_filename))
      return if File.exists?(compiled_filename) && !File.writable?(compiled_filename)
      FileUtils.mkdir_p(File.dirname(compiled_filename))
      File.open(compiled_filename, "wb") do |f|
        f.write(Sass::VERSION)
        f.write("\n")
        f.write(sha)
        f.write("\n")
        f.write(Marshal.dump(root))
      end
    end

    def find_full_path(filename, load_path)
      partial_name = File.join(File.dirname(filename), "_#{File.basename(filename)}")

      if Pathname.new(filename).absolute?
        [partial_name, filename].each do |name|
          return name if File.readable?(name)
        end
        return nil
      end

      [partial_name, filename].each do |name|
        full_path = File.join(load_path, name)
        return full_path if File.readable?(full_path)
      end
      nil
    end
  end
end
