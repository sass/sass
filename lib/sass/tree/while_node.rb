require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@while` loop.
  #
  # @see Sass::Tree
  class WhileNode < Node
    # The parse tree for the continuation expression.
    # @return [Script::Node]
    attr_reader :expr

    # @param expr [Script::Node] See \{#expr}
    def initialize(expr)
      @expr = expr
      super()
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@while #{@expr.to_sass(opts)}" + children_to_src(tabs, opts, fmt)
    end
  end
end
