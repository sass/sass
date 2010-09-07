require 'digest/sha1'
require 'pathname'
require 'fileutils'

module Sass
  # This module contains various bits of functionality
  # related to finding and caching Sass files.
  module Files
    extend self

    def tree_for(filename, options)
      engine_for(filename, options).to_tree
    end

    # Returns the {Sass::Tree} for the given file,
    # reading it from the Sass cache if possible.
    #
    # @param filename [String] The path to the Sass or SCSS file
    # @param options [{Symbol => Object}] The options hash.
    #   Only the {file:SASS_REFERENCE.md#cache-option `:cache_location`} option is used
    # @raise [Sass::SyntaxError] if there's an error in the document.
    #   The caller has responsibility for setting backtrace information, if necessary
    def engine_for(filename, options)
      had_syntax = options[:syntax]
      options = Sass::Engine.normalize_options(options)

      if had_syntax
        # Use what was explicitly specificed
      elsif filename =~ /\.scss$/
        options.merge!(:syntax => :scss)
      elsif filename =~ /\.sass$/
        options.merge!(:syntax => :sass)
      end

      options = options.merge(:filename => filename,
        :importer => options[:filesystem_importer].new("."))
      Sass::Engine.new(File.read(filename), options)
    end
  end
end
