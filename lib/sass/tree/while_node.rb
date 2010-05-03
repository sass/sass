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

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@while #{@expr.to_sass(opts)}" + children_to_src(tabs, opts, fmt)
    end

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

    # Returns an error message if the given child node is invalid,
    # and false otherwise.
    #
    # {ExtendNode}s are valid within {WhileNode}s.
    #
    # @param child [Tree::Node] A potential child node.
    # @return [Boolean, String] Whether or not the child node is valid,
    #   as well as the error message to display if it is invalid
    def invalid_child?(child)
      super unless child.is_a?(ExtendNode)
    end
  end
end
