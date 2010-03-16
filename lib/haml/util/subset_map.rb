require 'set'

module Haml
  module Util
    # A map from sets to values.
    # A value is \{#\[]= set} by providing a set (the "set-set") and a value,
    # which is then recorded as corresponding to that set.
    # Values are \{#\[] accessed} by providing a set (the "get-set")
    # and returning all values that correspond to set-sets
    # that are subsets of the get-set.
    #
    # SubsetMap preserves the order of values as they're inserted.
    #
    # @example
    # ssm = SubsetMap.new
    # ssm[Set[1, 2]] = "Foo"
    # ssm[Set[2, 3]] = "Bar"
    # ssm[Set[1, 2, 3]] = "Baz"
    #
    # ssm[Set[1, 2, 3]] #=> ["Foo", "Bar", "Baz"]
    class SubsetMap
      # Creates a new, empty SubsetMap.
      def initialize
        @hash = {}
        @vals = []
      end

      # Whether or not this SubsetMap has any key-value pairs.
      #
      # @return [Boolean]
      def empty?
        @hash.empty?
      end

      # Associates a value with a set.
      # When `set` or any of its supersets is accessed,
      # `value` will be among the values returned.
      #
      # Note that if the same `set` is passed to this method multiple times,
      # all given `value`s will be associated with that `set`.
      #
      # This runs in `O(n)` time, where `n` is the size of `set`.
      #
      # @param set [Set] The set to use as the map key. May not be empty.
      # @param value [Object] The value to associate with `set`.
      # @raise [ArgumentError] If `set` is empty.
      def []=(set, value)
        raise ArgumentError.new("SubsetMap keys may not be empty.") if set.empty?

        index = @vals.size
        @vals << value
        set.each do |k|
          @hash[k] ||= []
          @hash[k] << [set, index]
        end
      end

      # Returns all values associated with subsets of `set`.
      #
      # In the worst case, this runs in `O(m*max(n, log m))` time,
      # where `n` is the size of `set`
      # and `m` is the number of assocations in the map.
      # However, unless many keys in the map overlap with `set`,
      # `m` will typically be much smaller.
      #
      # @param set [Set] The set to use as the map key.
      # @return [Array] All values associated with subsets of `set`,
      #   in insertion order.
      def [](set)
        res = set.map do |k|
          next unless subsets = @hash[k]
          subsets.map do |subset, index|
            next unless subset.subset?(set)
            index
          end
        end
        res.flatten!
        res.compact!
        res.uniq!
        res.sort!
        res.map! {|i| @vals[i]}
        res
      end
    end
  end
end
