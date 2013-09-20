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
        other.is_a?(List) && self.value == other.value &&
        self.separator == other.separator)
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
      precedence = Sass::Script::Parser.precedence_of(separator)
      value.reject {|e| e.is_a?(Null)}.map do |v|
        if v.is_a?(List) && Sass::Script::Parser.precedence_of(v.separator) <= precedence ||
            separator == :space && v.is_a?(Sass::Script::Tree::UnaryOperation) &&
            (v.operator == :minus || v.operator == :plus)
          "(#{v.to_sass(opts)})"
        else
          v.to_sass(opts)
        end
      end.join(sep_str(nil))
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

    private

    def sep_str(opts = self.options)
      return ' ' if separator == :space
      return ',' if opts && opts[:style] == :compressed
      ', '
    end
  end
end
