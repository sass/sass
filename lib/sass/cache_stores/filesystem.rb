require 'fileutils'

module Sass
  module CacheStores
    # A backend for the Sass cache using the filesystem.
    class Filesystem < Base
      # The directory where the cached files will be stored.
      #
      # @return [String]
      attr_accessor :cache_location

      # @param cache_location [String] see \{#cache\_location}
      def initialize(cache_location)
        @cache_location = cache_location
      end

      # @see Base#\_retrieve
      def _retrieve(key, version, sha)
        return unless File.readable?(path_to(key))
        contents = nil
        File.open(path_to(key), "rb") do |f|
          if f.readline("\n").strip == version && f.readline("\n").strip == sha
            return f.read
          end
        end
        File.unlink path_to(key)
        nil
      rescue EOFError, TypeError, ArgumentError => e
        Sass::Util.sass_warn "Warning. Error encountered while reading cache #{path_to(key)}: #{e}"
      end

      # @see Base#\_store
      def _store(key, version, sha, contents)
        # return unless File.writable?(File.dirname(@cache_location))
        # return if File.exists?(@cache_location) && !File.writable?(@cache_location)
        compiled_filename = path_to(key)
        # return if File.exists?(File.dirname(compiled_filename)) && !File.writable?(File.dirname(compiled_filename))
        # return if File.exists?(compiled_filename) && !File.writable?(compiled_filename)
        FileUtils.mkdir_p(File.dirname(compiled_filename))
        File.open(compiled_filename, "wb") do |f|
          f.puts(version)
          f.puts(sha)
          f.write(contents)
        end
      rescue Errno::EACCES
        #pass
      end

      private

      # Returns the path to a file for the given key.
      #
      # @param key [String]
      # @return [String] The path to the cache file.
      def path_to(key)
        File.join(cache_location, key)
      end
    end
  end
end
