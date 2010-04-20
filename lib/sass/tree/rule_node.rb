require 'pathname'

module Sass::Tree
  # A static node reprenting a CSS rule.
  #
  # @see Sass::Tree
  class RuleNode < Node
    # The character used to include the parent selector
    # @private
    PARENT = '&'

    # The CSS selectors for this rule.
    # Each string is a selector line, and the lines are meant to be separated by commas.
    # For example,
    #
    #     foo, bar, baz,
    #     bip, bop, bup
    #
    # would be
    #
    #     ["foo, bar, baz",
    #      "bip, bop, bup"]
    #
    # @return [Array<String>]
    attr_accessor :rules

    # The CSS selectors for this rule,
    # parsed for commas and parent-references.
    # It's only set once {Tree::Node#perform} has been called.
    #
    # It's an array of arrays of arrays.
    # The first level of arrays represents distinct lines in the Sass file;
    # the second level represents comma-separated selectors;
    # the third represents structure within those selectors,
    # currently only parent-refs (represented by `:parent`).
    # For example,
    #
    #     &.foo, bar, baz,
    #     bip, &.bop, bup
    #
    # would be
    #
    #     [[[:parent, "foo"], ["bar"], ["baz"]],
    #      [["bip"], [:parent, "bop"], ["bup"]]]
    #
    # @return [Array<Array<Array<String|Symbol>>>]
    attr_accessor :parsed_rules

    # @param rule [String] The first CSS rule. See \{#rules}
    def initialize(rule)
      @rules = [rule]
      super()
    end

    # Compares the contents of two rules.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && rules == other.rules && super
    end

    # Adds another {RuleNode}'s rules to this one's.
    #
    # @param node [RuleNode] The other node
    def add_rules(node)
      @rules += node.rules
    end

    # @return [Boolean] Whether or not this rule is continued on the next line
    def continued?
      @rules.last[-1] == ?,
    end

    # Computes the CSS for the rule.
    #
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @param super_rules [Array<Array<String>>] The rules for the parent node
    #   (see \{#rules}), or `nil` if there are no parents
    # @return [String] The resulting CSS
    # @raise [Sass::SyntaxError] if the rule has no parents but uses `&`
    def to_s(tabs, super_rules = nil)
      resolved_rules = resolve_parent_refs(super_rules)

      properties = []
      sub_rules = []

      rule_separator = style == :compressed ? ',' : ', '
      line_separator = [:nested, :expanded].include?(style) ? ",\n" : rule_separator
      rule_indent = '  ' * (tabs - 1)
      per_rule_indent, total_indent = [:nested, :expanded].include?(style) ? [rule_indent, ''] : ['', rule_indent]

      total_rule = total_indent + resolved_rules.map do |line|
        per_rule_indent + line.join(rule_separator)
      end.join(line_separator)

      children.each do |child|
        next if child.invisible?
        if child.is_a? RuleNode
          sub_rules << child
        else
          properties << child
        end
      end

      to_return = ''
      if !properties.empty?
        old_spaces = '  ' * (tabs - 1)
        spaces = '  ' * tabs
        if @options[:line_comments] && style != :compressed
          to_return << "#{old_spaces}/* line #{line}"

          if filename
            relative_filename = if @options[:css_filename]
              begin
                Pathname.new(filename).relative_path_from(  
                  Pathname.new(File.dirname(@options[:css_filename]))).to_s
              rescue ArgumentError
                nil
              end
            end
            relative_filename ||= filename
            to_return << ", #{relative_filename}"
          end

          to_return << " */\n"
        end

        if style == :compact
          properties = properties.map { |a| a.to_s(1) }.select{|a| a && a.length > 0}.join(' ')
          to_return << "#{total_rule} { #{properties} }\n"
        elsif style == :compressed
          properties = properties.map { |a| a.to_s(1) }.select{|a| a && a.length > 0}.join(';')
          to_return << "#{total_rule}{#{properties}}"
        else
          properties = properties.map { |a| a.to_s(tabs + 1) }.select{|a| a && a.length > 0}.join("\n")
          end_props = (style == :expanded ? "\n" + old_spaces : ' ')
          to_return << "#{total_rule} {\n#{properties}#{end_props}}\n"
        end
      end

      tabs += 1 unless properties.empty? || style != :nested
      sub_rules.each do |sub|
        to_return << sub.to_s(tabs, resolved_rules)
      end

      to_return
    end

    protected

    # Runs any SassScript that may be embedded in the rule,
    # and parses the selectors for commas.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    def perform!(environment)
      @parsed_rules = @rules.map {|r| parse_selector(interpolate(r, environment))}
      super
    end

    private

    def resolve_parent_refs(super_rules)
      if super_rules.nil?
        return @parsed_rules.map do |line|
          line.map do |rule|
            if rule.include?(:parent)
              raise Sass::SyntaxError.new("Base-level rules cannot contain the parent-selector-referencing character '#{PARENT}'.", self.line)
            end

            rule.join
          end.compact
        end
      end

      new_rules = []
      super_rules.each do |super_line|
        @parsed_rules.each do |line|
          new_rules << []

          super_line.each do |super_rule|
            line.each do |rule|
              rule = [:parent, " ", *rule] unless rule.include?(:parent)

              new_rules.last << rule.map do |segment|
                next segment unless segment == :parent
                super_rule
              end.join
            end
          end
        end
      end
      new_rules
    end

    def parse_selector(text)
      scanner = StringScanner.new(text)
      rules = [[]]

      while scanner.rest?
        rules.last << scanner.scan(/[^",&]*/)
        case scanner.scan(/./)
        when '&'
          warn <<END unless rules.last.empty? || rules.last.last =~ /(^|\s)$/
DEPRECATION WARNING:
On line #{@line}#{" of '#{@filename}'" if @filename}
In Sass 3, parent selectors will only be able to appear
at the beginning of simple selector sequences.
For example, ".foo &.bar" is allowed but ".bar&" is not.
END
          rules.last << :parent
        when ','
          scanner.scan(/\s*/)
          rules << [] if scanner.rest?
        when '"'
          rules.last << '"' << scanner.scan(/([^"\\]|\\.)*/)
          # We don't want to enforce that strings are closed,
          # but we do want to consume quotes or trailing backslashes.
          rules.last << scanner.scan(/./) if scanner.rest?
        end
      end

      rules.map! do |l|
        Haml::Util.merge_adjacent_strings(l).reject {|r| r.is_a?(String) && r.empty?}
      end

      rules
    end
  end
end
