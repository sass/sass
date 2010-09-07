require 'digest/sha1'
require 'pathname'
require 'fileutils'

module Sass
  # This module contains various bits of functionality
  # related to finding and caching Sass files.
  module Files
    extend self

    # Returns the {Sass::Engine} for the given file.
    # This is preferable to {Sass::Engine.new} when reading from a file
    # because it properly sets up the Engine's metadata,
    # enables parse-tree caching,
    # and infers the syntax from the filename.
    #
    # @param filename [String] The path to the Sass or SCSS file
    # @param options [{Symbol => Object}] The options hash.
    #   Only the {file:SASS_REFERENCE.md#cache-option `:cache_location`} option is used
    # @return [Sass::Engine] The Engine for the given Sass or SCSS file.
    # @raise [Sass::SyntaxError] if there's an error in the document.
    def engine_for(filename, options)
      had_syntax = options[:syntax]

      if had_syntax
        # Use what was explicitly specificed
      elsif filename =~ /\.scss$/
        options.merge!(:syntax => :scss)
      elsif filename =~ /\.sass$/
        options.merge!(:syntax => :sass)
      end

      Sass::Engine.new(File.read(filename), options.merge(:filename => filename))
    end
  end
end
