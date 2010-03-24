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
    # @param filename [String] The path to the Sass file
    # @param options [{Symbol => Object}] The options hash.
    #   Only the {file:SASS_REFERENCE.md#cache-option `:cache_location`} option is used
    # @raise [Sass::SyntaxError] if there's an error in the document
    def tree_for(filename, options)
      options = Sass::Engine::DEFAULT_OPTIONS.merge(options)
      text = File.read(filename)

      if options[:cache]
        compiled_filename = sassc_filename(filename, options)
        sha = Digest::SHA1.hexdigest(text)

        if root = try_to_read_sassc(filename, compiled_filename, sha)
          root.options = options.merge(:filename => filename)
          return root
        end
      end

      engine = Sass::Engine.new(text, options.merge(:filename => filename))

      begin
        root = engine.to_tree
      rescue Sass::SyntaxError => err
        err.add_backtrace_entry(filename)
        raise err
      end

      try_to_write_sassc(root, compiled_filename, sha, options) if options[:cache]

      root
    end

    # Find the full filename of a Sass or CSS file to import.
    # This follows Sass's import rules:
    # if the filename given ends in `".sass"` or `".css"`,
    # it will try to find that type of file;
    # otherwise, it will try to find the corresponding Sass file
    # and fall back on CSS if it's not available.
    #
    # Any Sass filename returned will correspond to
    # an actual Sass file on the filesystem.
    # CSS filenames, however, may not;
    # they're expected to be put through directly to the stylesheet
    # as CSS `@import` statements.
    #
    # @param filename [String] The filename to search for
    # @param load_paths [Array<String>] The set of filesystem paths
    #   to search for Sass files.
    # @return [String] The filename of the imported file.
    #   This is an absolute path if the file is a `".sass"` file.
    # @raise [Sass::SyntaxError] if `filename` ends in ``".sass"``
    #   and no corresponding Sass file could be found.
    def find_file_to_import(filename, load_paths)
      was_sass = false
      original_filename = filename

      if filename[-5..-1] == ".sass"
        filename = filename[0...-5]
        was_sass = true
      elsif filename[-4..-1] == ".css"
        return filename
      end

      new_filename = find_full_path("#{filename}.sass", load_paths)

      return new_filename if new_filename
      unless was_sass
        warn <<END
WARNING: #{filename}.sass not found. Using #{filename}.css instead.
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

      raise SyntaxError.new(message, @line)
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
      warn "Warning. Error encountered while reading cache #{compiled_filename}: #{e}"
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

    def find_full_path(filename, load_paths)
      partial_name = File.join(File.dirname(filename), "_#{File.basename(filename)}")

      if Pathname.new(filename).absolute?
        [partial_name, filename].each do |name|
          return name if File.readable?(name)
        end
        return nil
      end

      load_paths.each do |path|
        [partial_name, filename].each do |name|
          full_path = File.join(path, name)
          if File.readable?(full_path)
            return full_path
          end
        end
      end
      nil
    end
  end
end
