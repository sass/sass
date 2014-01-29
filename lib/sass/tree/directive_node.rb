require 'sass/scss/rx'

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
    DIRECTIVE_NAME = /^@(?:-#{Sass::SCSS::RX::IDENT}-)?(#{Sass::SCSS::RX::NMCHAR}+)/

    # The text of the directive, `@` and all, with interpolation included.
    #
    # @return [Array<String, Sass::Script::Tree::Node>]
    attr_accessor :value

    # The text of the directive after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_value

    # @see RuleNode#tabs
    attr_accessor :tabs

    # @see RuleNode#group_end
    attr_accessor :group_end

    # @param value [Array<String, Sass::Script::Tree::Node>] See \{#value}
    def initialize(value)
      @value = value
      @tabs = 0
      super()
    end

    # @param value [String] See \{#resolved_value}
    # @return [DirectiveNode]
    def self.resolved(value)
      node = new([value])
      node.resolved_value = value
      node
    end

    # @return [String] The name of the directive, excluding `@` and any vendor prefixes.
    def name
      @name ||= value.first[DIRECTIVE_NAME, 1]
    end

    # @return [String] The name of the directive, excluding `@` but including vendor prefixes.
    def prefixed_name
      @prefixed_name ||= value.first[/^@([^ ]+)/, 1]
    end

    def bubbles?
      has_children
    end
  end
end
