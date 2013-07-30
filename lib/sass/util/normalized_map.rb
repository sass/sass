require 'delegate'
require 'sass/util'

module Sass
  module Util
    # A hash that normalizes its string keys while still allowing you to get back
    # to the original keys that were stored. If several different values normalize
    # to the same value, whichever is stored last wins.
    require 'sass/util/ordered_hash' if ruby1_8?
    class NormalizedMap < DelegateClass(ruby1_8? ? OrderedHash : Hash)
      # Create a normalized map
      def initialize
        @key_strings = {}
        @map = Util.ruby1_8? ? OrderedHash.new : {}

        # We delegate all hash methods that are not overridden here to @map.
        super(@map)
      end

      # Specifies how to transform the key.
      #
      # This can be overridden to create other normalization behaviors.
      def normalize(key)
        key.tr("-", "_")
      end

      # @private
      def []=(k, v)
        normalized = normalize(k)
        super(normalized, v)
        @key_strings[normalized] = k
      end

      # @private
      def [](k)
        super(normalize(k))
      end

      # @private
      def has_key?(k)
        super(normalize(k))
      end

      # @private
      def delete(k)
        normalized = normalize(k)
        @key_strings.delete(normalized)
        super(normalized)
      end

      # @return [Hash] Hash with the keys as they were stored (before normalization).
      def as_stored
        Sass::Util.map_keys(@map) {|k| @key_strings[k]}
      end

      # this is magically invoked by ruby, not sure why DelegateClass doesn't take care of it.
      # @private
      def initialize_dup(other)
        super
        @map = other.instance_variable_get("@map").dup
        __setobj__(@map)
      end
    end
  end
end
