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

    # @see Node#_around_dump
    def _around_dump
      old_else = @else
      old_last_else = @last_else
      @else = Sass::Util.dump(@else)
      @last_else = (self == @last_else ? nil : Sass::Util.dump(@last_else))
      super
    ensure
      @else = old_else
      @last_else = old_last_else
    end

    # @see Node#_after_load
    def _after_load
      super
      @else = Sass::Util.load(@else)
      @last_else = (@last_else ? Sass::Util.load(@last_else) : self)
    end
  end
end
