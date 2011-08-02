require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@while` loop.
  #
  # @see Sass::Tree
  class WhileNode < Node
    # The parse tree for the continuation expression.
    # @return [Script::Node]
    attr_accessor :expr

    # @param expr [Script::Node] See \{#expr}
    def initialize(expr)
      @expr = expr
      super()
    end
  end
end
