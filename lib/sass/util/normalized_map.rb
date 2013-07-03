require 'delegate'

module Sass
  module Util
    # A hash that normalizes its string keys while still allowing you to get back
    # to the original keys that were stored. If several different values normalize
    # to the same value, whichever is stored last wins.
    class NormalizedMap < DelegateClass(Hash)

      # Create a normalized map
      #
      # @param hash {String => Object} A hash to normalize and use
      #   as the initial values for this map.
      def initialize(hash = nil)
        @key_strings = {}
        @map = {}

        # We delegate all hash methods that are not overridden here to @map.
        super(@map)

        hash.each {|k, v| self[k] = v } if hash
      end

      # Specifies how to transform the key.
      #
      # This can be overridden to create other normalization behaviors
      def normalize(key)
        key.tr("-","_")
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

      # Returns the hash with the keys as they were stored (before normalization)
      def as_stored
        Sass::Util.map_keys(@map) {|k| @key_strings[k] }
      end

      # @private
      def deep_copy
        copy = self.class.new
        copy.instance_variable_set("@key_strings", @key_strings.dup)
        new_map = Sass::Util.map_vals(@map) {|v| v.respond_to?(:deep_copy) ? v.deep_copy : (v.dup rescue v)}
        copy.instance_variable_set("@map", new_map)
        copy.__setobj__(new_map)
        copy
      end

    end
  end
end
