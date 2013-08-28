module Sass::Script::Tree
  # A parse tree node representing a list literal. When resolved, this returns a
  # {Sass::Tree::Value::List}.
  class ListLiteral < Node
    # The parse nodes for members of this list.
    #
    # @return [Array<Node>]
    attr_reader :elements

    # The operator separating the values of the list. Either `:comma` or
    # `:space`.
    #
    # @return [Symbol]
    attr_reader :separator

    # Creates a new list literal.
    #
    # @param elements [Array<Node>] See \{#elements}
    # @param separator [Symbol] See \{#separator}
    def initialize(elements, separator)
      @elements = elements
      @separator = separator
    end

    # @see Node#children
    def children; elements; end

    # @see Value#to_sass
    def to_sass(opts = {})
      return "()" if elements.empty?
      precedence = Sass::Script::Parser.precedence_of(separator)
      elements.reject {|e| e.is_a?(Sass::Script::Value::Null)}.map do |v|
        if v.is_a?(ListLiteral) && Sass::Script::Parser.precedence_of(v.separator) <= precedence ||
            separator == :space && v.is_a?(UnaryOperation) && (v.operator == :minus || v.operator == :plus)
          "(#{v.to_sass(opts)})"
        else
          v.to_sass(opts)
        end
      end.join(sep_str(nil))
    end


    # @see Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@elements', elements.map {|e| e.deep_copy})
      node
    end

    def inspect
      "(#{elements.map {|e| e.inspect}.join(separator == :space ? ' ' : ', ')})"
    end

    protected

    def _perform(environment)
      list = Sass::Script::Value::List.new(
        elements.map {|e| e.perform(environment)},
        separator)
      list.line = line
      list.source_range = source_range
      list.filename = filename
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
