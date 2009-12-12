module Sass::Script
  # The abstract superclass for SassScript parse tree nodes.
  #
  # Use \{#perform} to evaluate a parse tree.
  class Node
    # The options hash for this node.
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # Sets the options hash for this node,
    # as well as for all child nodes.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @param options [{Symbol => Object}] The options
    def options=(options)
      @options = options
      children.each {|c| c.options = options}
    end

    # Evaluates the node.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the SassScript
    def perform(environment)
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #perform.")
    end

    # Returns all child nodes of this node.
    #
    # @return [Array<Node>]
    def children
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #children.")
    end
  end
end
