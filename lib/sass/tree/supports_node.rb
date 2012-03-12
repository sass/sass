module Sass::Tree
  # A static node representing a `@supports` rule.
  # `@supports` rules behave differently from other directives
  # in that when they're nested within rules,
  # they bubble up to top-level.
  #
  # @see Sass::Tree
  class SupportsNode < DirectiveNode
    # The name, which may include a browser prefix.
    #
    # @return [String]
    attr_accessor :name

    # The supports condition.
    #
    # @return [Sass::Supports::Condition]
    attr_accessor :condition

    # @see RuleNode#tabs
    attr_accessor :tabs

    # @see RuleNode#group_end
    attr_accessor :group_end

    # @param condition [Sass::Supports::Condition] See \{#condition}
    def initialize(name, condition)
      @name = name
      @condition = condition
      @tabs = 0
      super('')
    end

    # @see DirectiveNode#value
    def value; raise NotImplementedError; end

    # @see DirectiveNode#resolved_value
    def resolved_value
      @resolved_value ||= "@#{name} #{condition.to_css}"
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
