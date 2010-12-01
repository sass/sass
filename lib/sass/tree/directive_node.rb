module Sass::Tree
  # A static node representing an unproccessed Sass `@`-directive.
  # Directives known to Sass, like `@for` and `@debug`,
  # are handled by their own nodes;
  # only CSS directives like `@media` and `@font-face` become {DirectiveNode}s.
  #
  # `@import` and `@charset` are special cases;
  # they become {ImportNode}s and {CharsetNode}s, respectively.
  #
  # @see Sass::Tree
  class DirectiveNode < Node
    # The text of the directive, `@` and all.
    #
    # @return [String]
    attr_accessor :value

    # @param value [String] See \{#value}
    def initialize(value)
      @value = value
      super()
    end
  end
end
