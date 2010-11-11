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
      super("@media #{query}")
    end

    # Pass on the parent if it's a RuleNode.
    #
    # @see Node#cssize
    def cssize(extends, parent = nil)
      _cssize(extends, (parent if [MediaNode, RuleNode].include?(parent.class)))
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:filename => filename, :line => line)
      raise e
    end

    protected

    # If we're passed a parent, bubble it down.
    #
    # @see Node#cssize
    def cssize!(extends, parent)
      return super unless parent
      new_rule = parent.dup
      new_rule.children = self.children
      self.children = Array(new_rule.cssize(extends, self))
    end

    # @see Node#to_s
    def _to_s(tabs)
      str = super(tabs + self.tabs)
      str.gsub!(/\n\Z/, '') unless style == :compressed || group_end
      str
    end
  end
end
