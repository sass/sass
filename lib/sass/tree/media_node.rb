module Sass::Tree
  # A static node representing a `@media` rule.
  # `@media` rules behave differently from other directives
  # in that when they're nested within rules,
  # they bubble up to top-level.
  #
  # @see Sass::Tree
  class MediaNode < DirectiveNode
    # TODO: parse and cache the query immediately if it has no dynamic elements

    # The media query for this rule, interspersed with {Sass::Script::Node}s
    # representing `#{}`-interpolation. Any adjacent strings will be merged
    # together.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :query

    # The media query for this rule, without any unresolved interpolation. It's
    # only set once {Tree::Node#perform} has been called.
    #
    # @return [Sass::Media::QueryList]
    attr_accessor :resolved_query

    # @see RuleNode#tabs
    attr_accessor :tabs

    # @see RuleNode#group_end
    attr_accessor :group_end

    # @param query [Array<String, Sass::Script::Node>] See \{#query}
    def initialize(query)
      @query = query
      @tabs = 0
      super('')
    end

    # @see DirectiveNode#value
    def value; raise NotImplementedError; end

    # @see DirectiveNode#name
    def name; '@media'; end

    # @see DirectiveNode#resolved_value
    def resolved_value
      @resolved_value ||= "@media #{resolved_query.to_css}"
    end

    # True when the directive has no visible children.
    #
    # @return [Boolean]
    def invisible?
      children.all? {|c| c.invisible?}
    end

    # @see Node#bubbles?
    def bubbles?; true; end
  end
end
