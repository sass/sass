module Sass::Script::Value
  # A SassScript object representing a CSS list.
  # This includes both comma-separated lists and space-separated lists.
  class List < Base
    # The Ruby array containing the contents of the list.
    #
    # @return [Array<Value>]
    attr_reader :value
    alias_method :to_a, :value

    # The operator separating the values of the list.
    # Either `:comma` or `:space`.
    #
    # @return [Symbol]
    attr_reader :separator

    # Creates a new list.
    #
    # @param value [Array<Value>] See \{#value}
    # @param separator [Symbol] See \{#separator}
    def initialize(value, separator)
      super(value)
      @separator = separator
    end

    # @see Value#options=
    def options=(options)
      super
      value.each {|v| v.options = options}
    end

    # @see Value#eq
    def eq(other)
      Sass::Script::Value::Bool.new(
        other.is_a?(List) && value == other.value &&
        separator == other.separator)
    end

    def hash
      @hash ||= [value, separator].hash
    end

    # @see Value#to_s
    def to_s(opts = {})
      raise Sass::SyntaxError.new("() isn't a valid CSS value.") if value.empty?
      value.
        reject {|e| e.is_a?(Null) || e.is_a?(List) && e.value.empty?}.
        map {|e| e.to_s(opts)}.join(sep_str)
    end

    # @see Value#to_sass
    def to_sass(opts = {})
      return "()" if value.empty?
      members = value.map do |v|
        if element_needs_parens?(v)
          "(#{v.to_sass(opts)})"
        else
          v.to_sass(opts)
        end
      end
      return "(#{members.first},)" if members.length == 1 && separator == :comma
      members.join(sep_str(nil))
    end

    # @see Value#to_h
    def to_h
      return Sass::Util.ordered_hash if value.empty?
      return @map ||= Sass::Util.to_hash(value.map {|e| e.to_a}) if is_pseudo_map?
      super
    end

    # Returns whether a warning still needs to be printed for this list being used as a map.
    #
    # @return [Boolean]
    def needs_map_warning?
      !@value.empty? && !@map
    end

    # Returns whether this is a list of pairs that can be used as a map.
    #
    # @return [Boolean]
    def is_pseudo_map?
      @is_pseudo_map ||= value.all? {|e| e.is_a?(Sass::Script::Value::List) && e.to_a.length == 2}
    end

    # @see Value#inspect
    def inspect
      "(#{value.map {|e| e.inspect}.join(sep_str(nil))})"
    end

    # Asserts an index is within the list.
    #
    # @private
    #
    # @param list [Sass::Script::Value::List] The list for which the index should be checked.
    # @param n [Sass::Script::Value::Number] The index being checked.
    def self.assert_valid_index(list, n)
      if !n.int? || n.to_i == 0
        raise ArgumentError.new("List index #{n} must be a non-zero integer")
      elsif list.to_a.size == 0
        raise ArgumentError.new("List index is #{n} but list has no items")
      elsif n.to_i.abs > (size = list.to_a.size)
        raise ArgumentError.new(
          "List index is #{n} but list is only #{size} item#{'s' if size != 1} long")
      end
    end

    private

    def element_needs_parens?(element)
      if element.is_a?(List)
        return false if element.value.empty?
        precedence = Sass::Script::Parser.precedence_of(separator)
        return Sass::Script::Parser.precedence_of(element.separator) <= precedence
      end

      return false unless separator == :space
      return false unless element.is_a?(Sass::Script::Tree::UnaryOperation)
      element.operator == :minus || element.operator == :plus
    end

    def sep_str(opts = options)
      return ' ' if separator == :space
      return ',' if opts && opts[:style] == :compressed
      ', '
    end
  end
end
