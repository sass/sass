require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@while` loop.
  #
  # @see Sass::Tree
  class WhileNode < Node
    # @param expr [Script::Node] The parse tree for the continue expression
    def initialize(expr)
      @expr = expr
      super()
    end

    protected

    # Runs the child nodes until the continue expression becomes false.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @return [Array<Tree::Node>] The resulting static nodes
    # @see Sass::Tree
    def _perform(environment)
      children = []
      new_environment = Sass::Environment.new(environment)
      while @expr.perform(environment).to_bool
        children += perform_children(new_environment)
      end
      children
    end
  end
end
