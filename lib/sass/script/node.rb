module Sass::Script
  # The abstract superclass for SassScript parse tree nodes.
  #
  # Use \{#perform} to evaluate a parse tree.
  class Node
    # Evaluates the node.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the SassScript
    def perform(environment)
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #perform.")
    end
  end
end
