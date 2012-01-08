require 'sass/tree/node'

module Sass::Tree
  # A static node reprenting an `@extend` directive.
  #
  # @see Sass::Tree
  class ExtendNode < Node
    # The parsed selector after interpolation has been resolved.
    # Only set once {Tree::Visitors::Perform} has been run.
    #
    # @return [Selector::CommaSequence]
    attr_accessor :resolved_selector

    # The CSS selector to extend, interspersed with {Sass::Script::Node}s
    # representing `#{}`-interpolation.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :selector

    # Whether this extends is optional.
    # Optional extends do not issue a warning when the base selector is not found.
    #
    # @return [Boolean]
    attr_accessor :optional

    # @param selector [Array<String, Sass::Script::Node>]
    #   The CSS selector to extend,
    #   interspersed with {Sass::Script::Node}s
    #   representing `#{}`-interpolation.
    def initialize(selector, optional)
      @selector = selector
      @optional = optional
      super()
    end
  end
end
