module Sass::Tree
  # A static node representing a `@media` rule.
  # `@media` rules behave differently from other directives
  # in that when they're nested within rules,
  # they bubble up to top-level.
  #
  # @see Sass::Tree
  class MediaNode < DirectiveNode
    # The media query (e.g. `print` or `screen`).
    #
    # @return [String]
    attr_accessor :query

    # @see RuleNode#tabs
    attr_accessor :tabs

    # @see RuleNode#group_end
    attr_accessor :group_end

    # @param query [String] See \{#query}
    def initialize(query)
      @query = query
      @tabs = 0
      super('')
    end

    # @see DirectiveNode#value
    def value
      "@media #{query}"
    end
  end
end
