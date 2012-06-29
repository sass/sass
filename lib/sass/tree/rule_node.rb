require 'pathname'
require 'uri'

module Sass::Tree
  # A static node reprenting a CSS rule.
  #
  # @see Sass::Tree
  class RuleNode < Node
    # The character used to include the parent selector
    PARENT = '&'

    # The CSS selector for this rule,
    # interspersed with {Sass::Script::Node}s
    # representing `#{}`-interpolation.
    # Any adjacent strings will be merged together.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :rule

    # The CSS selector for this rule,
    # without any unresolved interpolation
    # but with parent references still intact.
    # It's only set once {Tree::Node#perform} has been called.
    #
    # @return [Selector::CommaSequence]
    attr_accessor :parsed_rules

    # The CSS selector for this rule,
    # without any unresolved interpolation or parent references.
    # It's only set once {Tree::Visitors::Cssize} has been run.
    #
    # @return [Selector::CommaSequence]
    attr_accessor :resolved_rules

    # How deep this rule is indented
    # relative to a base-level rule.
    # This is only greater than 0 in the case that:
    #
    # * This node is in a CSS tree
    # * The style is :nested
    # * This is a child rule of another rule
    # * The parent rule has properties, and thus will be rendered
    #
    # @return [Fixnum]
    attr_accessor :tabs

    # Whether or not this rule is the last rule in a nested group.
    # This is only set in a CSS tree.
    #
    # @return [Boolean]
    attr_accessor :group_end

    # The stack trace.
    # This is only readable in a CSS tree as it is written during the perform step
    # and only when the :trace_selectors option is set.
    #
    # @return [Array<String>]
    attr_accessor :stack_trace

    # @param rule [Array<String, Sass::Script::Node>]
    #   The CSS rule. See \{#rule}
    def initialize(rule)
      merged = Sass::Util.merge_adjacent_strings(rule)
      @rule = Sass::Util.strip_string_array(merged)
      @tabs = 0
      try_to_parse_non_interpolated_rules
      super()
    end

    # If we've precached the parsed selector, set the line on it, too.
    def line=(line)
      @parsed_rules.line = line if @parsed_rules
      super
    end

    # If we've precached the parsed selector, set the filename on it, too.
    def filename=(filename)
      @parsed_rules.filename = filename if @parsed_rules
      super
    end

    # Compares the contents of two rules.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && rule == other.rule && super
    end

    # Adds another {RuleNode}'s rules to this one's.
    #
    # @param node [RuleNode] The other node
    def add_rules(node)
      @rule = Sass::Util.strip_string_array(
        Sass::Util.merge_adjacent_strings(@rule + ["\n"] + node.rule))
      try_to_parse_non_interpolated_rules
    end

    # @return [Boolean] Whether or not this rule is continued on the next line
    def continued?
      last = @rule.last
      last.is_a?(String) && last[-1] == ?,
    end

    # A hash that will be associated with this rule in the CSS document
    # if the {file:SASS_REFERENCE.md#debug_info-option `:debug_info` option} is enabled.
    # This data is used by e.g. [the FireSass Firebug extension](https://addons.mozilla.org/en-US/firefox/addon/103988).
    #
    # @return [{#to_s => #to_s}]
    def debug_info
      {:filename => filename && ("file://" + URI.escape(File.expand_path(filename))),
       :line => self.line}
    end

    # A rule node is invisible if it has only placeholder selectors.
    def invisible?
      resolved_rules.members.all? {|seq| seq.has_placeholder?}
    end

    private

    def try_to_parse_non_interpolated_rules
      if @rule.all? {|t| t.kind_of?(String)}
        # We don't use real filename/line info because we don't have it yet.
        # When we get it, we'll set it on the parsed rules if possible.
        parser = Sass::SCSS::StaticParser.new(@rule.join.strip, '', 1)
        @parsed_rules = parser.parse_selector rescue nil
      end
    end
  end
end
