require 'delegate'
require 'sass/util'

module Sass
  module Util
    # A hash that normalizes its string keys while still allowing you to get back
    # to the original keys that were stored. If several different values normalize
    # to the same value, whichever is stored last wins.
    require 'sass/util/ordered_hash' if ruby1_8?
    class NormalizedMap
      # Create a normalized map
      def initialize
        @key_strings = {}
        @map = Util.ruby1_8? ? OrderedHash.new : {}
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
        @map[normalized] = v
        @key_strings[normalized] = k
        v
      end

      # @private
      def [](k)
        @map[normalize(k)]
      end

      # @private
      def has_key?(k)
        @map.has_key?(normalize(k))
      end

      # @private
      def delete(k)
        normalized = normalize(k)
        @key_strings.delete(normalized)
        @map.delete(normalized)
      end

      # @return [Hash] Hash with the keys as they were stored (before normalization).
      def as_stored
        Sass::Util.map_keys(@map) {|k| @key_strings[k]}
      end

      def empty?
        @map.empty?
      end

      def values
        @map.values
      end

      def keys
        @map.keys
      end

      def each
        @map.each {|k, v| yield(k, v)}
      end

      def size
        @map.size
      end

      def to_hash
        @map.dup
      end

      def to_a
        @map.to_a
      end

      def map
        @map.map {|h, k| yield(h, k)}
      end

      def dup
        d = super
        d.send(:instance_variable_set, "@map", @map.dup)
        d
      end
    end
  end
end
