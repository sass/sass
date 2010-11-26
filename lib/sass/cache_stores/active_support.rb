module Sass
  module CacheStores
    # A cache store that wraps an ActiveSupport cache store.
    # This is useful for integrating with an app that uses ActiveSupport,
    # or for taking advantage of the wide variety of ActiveSupport cache backends.
    #
    # This is automatically used within Rails.
    class ActiveSupport < Base
      # @param store [::ActiveSupport::Cache::Store] The cache store to wrap.
      def initialize(store)
        @store = store
      end

      # @see Base#_retrieve
      def _retrieve(key, version, sha)
        return unless val = @store.fetch('_sass/' + key)
        return unless val[:version] == version
        return unless val[:sha] == sha
        return val[:contents]
      end

      # @see Base#_store
      def _store(key, version, sha, contents)
        @store.write('_sass/' + key, :version => version, :sha => sha, :contents => contents)
      end
    end
  end
end
