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
      had_syntax = options[:syntax]
      options = Sass::Engine.normalize_options(options)
      text = File.read(filename)

      if options[:cache] || options[:read_cache]
        key = sassc_key(filename, options)
        sha = Digest::SHA1.hexdigest(text)

        if root = options[:cache_store].retrieve(key, sha)
          root.options = root.options.merge(options.merge(:filename => filename))
          return root
        end
      end

      if had_syntax
        # Use what was explicitly specificed
      elsif filename =~ /\.scss$/
        options.merge!(:syntax => :scss)
      elsif filename =~ /\.sass$/
        options.merge!(:syntax => :sass)
      end

      options = options.merge(:filename => filename,
        :importer => Importers::Base.default_filesystem_class.new("."))
      engine = Sass::Engine.new(text, options)

      root = engine.to_tree
      options[:cache_store].store(key, sha, root) if options[:cache]
      root
    end

    private

    def sassc_key(filename, options)
      dir = File.dirname(File.expand_path(filename))
      options[:cache_store].key(dir, File.basename(filename))
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
