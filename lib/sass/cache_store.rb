require 'stringio'

module Sass
  # An abstract base class for backends for the Sass cache.
  # Any key-value store can act as such a backend;
  # it just needs to implement the
  # \{#_store} and \{#_retrieve} methods.
  #
  # To use a cache store with Sass,
  # use the {file:SASS_REFERENCE.md#cache_store-option `:cache_store` option}.
  class CacheStore
    # Store cached contents for later retrieval
    # Must be implemented by all CacheStore subclasses
    #
    # Note: cache contents contain binary data.
    #
    # @param key [String] The key to store the contents under
    # @param version [String] The current sass version.
    #                Cached contents must not be retrieved across different versions of sass.
    # @param sha [String] The sha of the sass source.
    #                Cached contents must not be retrieved if the sha has changed.
    # @param contents [String] The contents to store.
    def _store(key, version, sha, contents)
      raise "#{self.class} must implement #_store."
    end

    # Retrieved cached contents.
    # Must be implemented by all subclasses.
    # 
    # Note: if the key exists but the sha or version have changed,
    # then the key may be deleted by the cache store, if it wants to do so.
    #
    # @param key [String] The key to retrieve
    # @param version [String] The current sass version.
    #                Cached contents must not be retrieved across different versions of sass.
    # @param sha [String] The sha of the sass source.
    #                Cached contents must not be retrieved if the sha has changed.
    # @return [String] The contents that were previously stored.
    # @return [NilClass] when the cache key is not found or the version or sha have changed.
    def _retrieve(key, version, sha)
      raise "#{self.class} must implement #_retrieve."
    end

    # Store a {Sass::Tree::RootNode}.
    #
    # @param key [String] The key to store it under.
    # @param sha [String] The checksum for the contents that are being stored.
    # @param root [Sass::Tree::RootNode] The root of the tree to be stored.
    def store(key, sha, root)
      orig_options = root.options
      begin
        _store(key, Sass::VERSION, sha, Haml::Util.dump(root))
      ensure
        root.options = orig_options
      end
    end

    # Retrieve a {Sass::Tree::RootNode}.
    #
    # @param key [String] The key the root element was stored under.
    # @param sha [String] The checksum of the root element's content.
    # @return [Sass::Tree::RootNode] The root node.
    def retrieve(key, sha)
      contents = _retrieve(key, Sass::VERSION, sha)
      Haml::Util.load(contents) if contents
    rescue EOFError, TypeError, ArgumentError => e
      raise
      Haml::Util.haml_warn "Warning. Error encountered while reading cache #{path_to(key)}: #{e}"
    end

    # Return the key for the sass file.
    #
    # The `(sass_dirname, sass_basename)` pair
    # should uniquely identify the Sass document,
    # but otherwise there are no restrictions on their content.
    #
    # @param sass_dirname [String]
    #   The fully-expanded location of the Sass file.
    #   This corresponds to the directory name on a filesystem.
    # @param sass_basename [String] The name of the Sass file that is being referenced.
    #   This corresponds to the basename on a filesystem.
    def key(sass_dirname, sass_basename)
      dir = Digest::SHA1.hexdigest(sass_dirname)
      filename = "#{sass_basename}c"
      "#{dir}/#{filename}"
    end
  end

  # A backend for the Sass cache using the filesystem.
  class FileCacheStore < CacheStore
    # The directory where the cached files will be stored.
    #
    # @return [String]
    attr_accessor :cache_location

    # Create a new FileCacheStore.
    #
    # @param cache_location [String] see \{#cache\_location}
    def initialize(cache_location)
      @cache_location = cache_location
    end

    # @see {CacheStore#\_retrieve\_}
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
      Haml::Util.haml_warn "Warning. Error encountered while reading cache #{path_to(key)}: #{e}"
    end

    # @see {CacheStore#\_store\_}
    def _store(key, version, sha, contents)
      return unless File.writable?(File.dirname(@cache_location))
      return if File.exists?(@cache_location) && !File.writable?(@cache_location)
      compiled_filename = path_to(key)
      return if File.exists?(File.dirname(compiled_filename)) && !File.writable?(File.dirname(compiled_filename))
      return if File.exists?(compiled_filename) && !File.writable?(compiled_filename)
      FileUtils.mkdir_p(File.dirname(compiled_filename))
      File.open(compiled_filename, "wb") do |f|
        f.puts(version)
        f.puts(sha)
        f.write(contents)
      end
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

  # A backend for the Sass cache using in-process memory.
  class InMemoryCacheStore < CacheStore
    # Since the {InMemoryCacheStore} is stored in the Sass tree's options hash,
    # when the options get serialized as part of serializing the tree,
    # you get crazy exponential growth in the size of the cached objects
    # unless you don't dump the cache.
    #
    # @private
    def _dump(depth)
      ""
    end

    # If we deserialize this class, just make a new empty one.
    #
    # @private
    def self._load(repr)
      InMemoryCacheStore.new
    end

    # Create a new, empty cache store.
    def initialize
      @contents = {}
    end

    # @see CacheStore#_retrieve
    def _retrieve(key, version, sha)
      if @contents.has_key?(key)
        return unless @contents[key][:version] == version
        return unless @contents[key][:sha] == sha
        return @contents[key][:contents]
      end
    end
    
    # @see CacheStore#_store
    def _store(key, version, sha, contents)
      @contents[key] = {
        :version => version,
        :sha => sha,
        :contents => contents
      }
    end
    
    # Destructively clear the cache.
    def reset!
      @contents = {}
    end
  end

  # Doesn't store anything, but records what things it should have stored.
  # This doesn't currently have any use except for testing and debugging.
  #
  # @private
  class NullCacheStore < CacheStore
    def initialize
      @keys = {}
    end

    def _retrieve(key, version, sha)
      nil
    end
    
    def _store(key, version, sha, contents)
      @keys[key] = true
    end
    
    def was_set?(key)
      @keys[key]
    end
  end
end
