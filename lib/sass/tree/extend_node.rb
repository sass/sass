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

    # Whether the `@extend` is allowed to match no selectors or not.
    #
    # @return [Boolean]
    def optional?; @optional; end

    # @param selector [Array<String, Sass::Script::Node>]
    #   The CSS selector to extend,
    #   interspersed with {Sass::Script::Node}s
    #   representing `#{}`-interpolation.
    # @param optional [Boolean] See \{#optional}
    def initialize(selector, optional)
      @selector = selector
      @optional = optional
      super()
    end
  end
end
