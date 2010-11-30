require 'sass/tree/node'

module Sass::Tree
  # A static node reprenting an `@extend` directive.
  #
  # @see Sass::Tree
  class ExtendNode < Node
    # The parsed selector after interpolation has been resolved.
    # Only set once {Tree::Node#perform} has been called.
    #
    # @return [Selector::CommaSequence]
    attr_reader :resolved_selector

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

    # Runs SassScript interpolation in the selector,
    # and then parses the result into a {Sass::Selector::CommaSequence}.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    def perform!(environment)
      @resolved_selector = Sass::SCSS::CssParser.new(run_interp(@selector, environment), self.line).
        parse_selector(self.filename)
      super
    end
  end
end
