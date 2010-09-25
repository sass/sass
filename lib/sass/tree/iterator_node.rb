require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@for $var in $list` loop.
  #
  # @see Sass::Tree
  class IteratorNode < Node
    # @param var [String] The name of the loop variable
    # @param list [Script::Node] The parse tree for the initial expression
    def initialize(var, list)
      @var = var
      @list = list
      super()
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@for $#{dasherize(@var, opts)} in #{@list.to_sass(opts)}" +
        children_to_src(tabs, opts, fmt)
    end

    # Runs the child nodes once for each element in the list,
    # varying the variable each time.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @return [Array<Tree::Node>] The resulting static nodes
    # @see Sass::Tree
    def _perform(environment)
      list = @list.perform(environment)
      list.assert_list!

      children = []
      environment = Sass::Environment.new(environment)
      list.elements.each do |el|
        environment.set_local_var(@var, el)
        children += perform_children(environment)
      end
      children
    end

    # Returns an error message if the given child node is invalid,
    # and false otherwise.
    #
    # {ExtendNode}s are valid within {IteratorNode}s.
    #
    # @param child [Tree::Node] A potential child node.
    # @return [Boolean, String] Whether or not the child node is valid,
    #   as well as the error message to display if it is invalid
    def invalid_child?(child)
      super unless child.is_a?(ExtendNode)
    end
  end
end
