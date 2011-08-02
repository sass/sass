require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@if` statement.
  #
  # {IfNode}s are a little odd, in that they also represent `@else` and `@else if`s.
  # This is done as a linked list:
  # each {IfNode} has a link (\{#else}) to the next {IfNode}.
  #
  # @see Sass::Tree
  class IfNode < Node
    # The conditional expression.
    # If this is nil, this is an `@else` node, not an `@else if`.
    #
    # @return [Script::Expr]
    attr_reader :expr

    # The next {IfNode} in the if-else list, or `nil`.
    #
    # @return [IfNode]
    attr_accessor :else

    # @param expr [Script::Expr] See \{#expr}
    def initialize(expr)
      @expr = expr
      @last_else = self
      super()
    end

    # Append an `@else` node to the end of the list.
    #
    # @param node [IfNode] The `@else` node to append
    def add_else(node)
      @last_else.else = node
      @last_else = node
    end

    # @see Node#options=
    def options=(options)
      super
      self.else.options = options if self.else
    end

    def _dump(f)
      Marshal.dump([self.expr, self.else, self.children])
    end

    def self._load(data)
      expr, else_, children = Marshal.load(data)
      node = IfNode.new(expr)
      node.else = else_
      node.children = children
      node.instance_variable_set('@last_else',
        node.else ? node.else.instance_variable_get('@last_else') : node)
      node
    end

    # @see Node#deep_copy
    def deep_copy
      node = super
      node.else = self.else.deep_copy if self.else
      node
    end
  end
end
