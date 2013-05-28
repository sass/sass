module Sass::Script::Value
  # A SassScript object representing a CSS list.
  # This includes both comma-separated lists and space-separated lists.
  class List < Base
    # The Ruby array containing the contents of the list.
    #
    # @return [Array<Value>]
    attr_reader :value
    alias_method :children, :value
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

    # @see Sass::Script::Tree::Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@value', value.map {|c| c.deep_copy})
      node
    end

    # @see Sass::Script::Tree::Node#eq
    def eq(other)
      Sass::Script::Value::Bool.new(
        other.is_a?(List) && self.value == other.value &&
        self.separator == other.separator)
    end

    # @see Sass::Script::Tree::Node#to_s
    def to_s(opts = {})
      raise Sass::SyntaxError.new("() isn't a valid CSS value.") if value.empty?
      return value.reject {|e| e.is_a?(Null) || e.is_a?(List) && e.value.empty?}.map {|e| e.to_s(opts)}.join(sep_str)
    end

    # @see Sass::Script::Tree::Node#to_sass
    def to_sass(opts = {})
      return "()" if value.empty?
      precedence = Sass::Script::Parser.precedence_of(separator)
      value.reject {|e| e.is_a?(Null)}.map do |v|
        if v.is_a?(List) && Sass::Script::Parser.precedence_of(v.separator) <= precedence
          "(#{v.to_sass(opts)})"
        else
          v.to_sass(opts)
        end
      end.join(sep_str(nil))
    end

    # @see Sass::Script::Tree::Node#inspect
    def inspect
      "(#{value.map {|e| e.inspect}.join(sep_str(nil))})"
    end

    protected

    # @see Sass::Script::Tree::Node#_perform
    def _perform(environment)
      list = Sass::Script::Value::List.new(
        value.map {|e| e.perform(environment)},
        separator)
      list.options = self.options
      list
    end

    private

    def sep_str(opts = self.options)
      return ' ' if separator == :space
      return ',' if opts && opts[:style] == :compressed
      return ', '
    end
  end
end
