require 'stringio'
module Sass
  # An abstract base class for all cache stores.
  # At a minimum, subclasses should implement the
  # `_store_` and `_retrieve_` methods.
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
    def _store_(key, version, sha, contents)
      raise "Implement Me!"
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
    def _retrieve_(key, version, sha)
      raise "Implement Me!"
    end

    # stores a {Sass::Tree::RootNode}
    # @param key [String] The key to store it as
    # @param sha [String] The checksum for the contents that are being stored
    # @param root [Sass::Tree::RootNode] The root of the tree to be stored.
    def store(key, sha, root)
      orig_options = root.options
      begin
        root.options = {}
        _store_(key, Sass::VERSION, sha, Marshal.dump(root))
      ensure
        root.options = orig_options
      end
    end

    # retrieves a {Sass::Tree::RootNode}
    # @param key [String] The key the root element was stored as
    # @param sha [String] The checksum of the root element's content
    # @return [Sass::Tree::RootNode] The root node.
    def retrieve(key, sha)
      contents = _retrieve_(key, Sass::VERSION, sha)
      Marshal.load(contents) if contents
    rescue EOFError, TypeError, ArgumentError => e
      raise
      Haml::Util.haml_warn "Warning. Error encountered while reading cache #{path_to(key)}: #{e}"
    end

    # Return the key for the sass file.
    # @param real_sass_location [String] An identifier for the location where the original sass file is located.
    # @param sass_filename [String] The name of the sass file that is being referenced.
    def key(real_sass_location, sass_filename)
      dir = Digest::SHA1.hexdigest(real_sass_location)
      filename = "#{sass_filename}c"
      "#{dir}/#{filename}"
    end
  end

  # Stores the cached files to the filesystem.
  class FileCacheStore < CacheStore
    attr_accessor :cache_location

    # Create a new FileCacheStore.
    # @param cache_location [String] The path to the cache
    def initialize(cache_location)
      @cache_location = cache_location
    end

    # retrieves the key from disk.
    # @param key [String] The key to be retrieved.
    # @return [String] the contents of a cached sass file.
    def _retrieve_(key, version, sha)
      return unless File.readable?(path_to(key))
      contents = nil
      File.open(path_to(key), "rb") do |f|
        if (was_version = f.readline("\n").strip) == version && (was_sha = f.readline("\n").strip) == sha
          return f.read
        end
      end
      File.unlink path_to(key)
      nil
    rescue EOFError, TypeError, ArgumentError => e
      Haml::Util.haml_warn "Warning. Error encountered while reading cache #{path_to(key)}: #{e}"
    end

    # Stores the contents to disk under key.
    # @param key [String] The key to store. This key is written to the `cache_location` provided when creating the cache store.
    # @param contents [String] The contents to store.
    def _store_(key, version, sha, contents)
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
    # Returns the path to a file for the key given.
    def path_to(key)
      File.join(cache_location, key)
    end
  end

  # Store the sass cache in the process's memory
  class InMemoryCacheStore < CacheStore
    # @private
    # Since the object is stored in the options
    # when the options get serialized as part of serializing the tree,
    # you get crazy exponential growth in the size of the cached objects.
    def _dump(depth)
      ""
    end

    # @private
    # If we deserialize this class, just make a new empty one.
    def self._load(repr)
      InMemoryCacheStore.new
    end

    def initialize
      @contents = {}
    end

    def _retrieve_(key, version, sha)
      if @contents.has_key?(key)
        return unless @contents[key][:version] == version
        return unless @contents[key][:sha] == sha
        return @contents[key][:contents]
      end
    end
    
    def _store_(key, version, sha, contents)
      @contents[key] = {
        :version => version,
        :sha => sha,
        :contents => contents
      }
    end
    
    # Resets the cache
    def reset!
      @contents = {}
    end
  end

  # @private
  # Doesn't store anything, but records what things it should have stored.
  # This doesn't currently have any use except for testing and debugging.
  class NullCacheStore < CacheStore
    def initialize
      @keys = {}
    end
    def _retrieve_(key, version, sha)
      nil
    end
    
    def _store_(key, version, sha, contents)
      @keys[key] = true
    end
    
    def was_set?(key)
      @keys[key]
    end
  end

end
