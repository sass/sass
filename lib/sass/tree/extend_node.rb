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
    attr_reader :selector

    # @param selector [Array<String, Sass::Script::Node>]
    #   The CSS selector to extend,
    #   interspersed with {Sass::Script::Node}s
    #   representing `#{}`-interpolation.
    def initialize(selector)
      @selector = selector
      super()
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@extend #{selector_to_src(@selector, tabs, opts, fmt).lstrip}#{semi fmt}\n"
    end
  end
end
