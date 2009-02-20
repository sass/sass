require 'sass/tree/node'
require 'sass/tree/attr_node'

module Sass::Tree
  class RuleNode < ValueNode
    # The character used to include the parent selector
    PARENT = '&'

    alias_method :rule, :value
    alias_method :rule=, :value=

    def ==(other)
      self.class == other.class && rules == other.rules && super
    end

    def rules
      Array(rule)
    end

    def add_rules(node)
      self.rule = rules
      self.rule += node.rules
    end

    def continued?
      rule[-1] == ?,
    end

    def to_s(tabs, super_rules = nil)
      attributes = []
      sub_rules = []

      rule_split = /\s*,\s*/
      rule_separator = @style == :compressed ? ',' : ', '
      line_separator = [:nested, :expanded].include?(@style) ? ",\n" : rule_separator
      rule_indent = '  ' * (tabs - 1)
      total_rule = if super_rules
        super_rules.split(",\n").map do |super_line|
          super_line.strip.split(rule_split).map do |super_rule|
            self.rules.map do |line|
              rule_indent + line.gsub(/,$/, '').split(rule_split).map do |rule|
                if rule.include?(PARENT)
                  rule.gsub(PARENT, super_rule)
                else
                  "#{super_rule} #{rule}"
                end
              end.join(rule_separator)
            end.join(line_separator)
          end.join(rule_separator)
        end.join(line_separator)
      elsif self.rules.any? { |r| r.include?(PARENT) }
        raise Sass::SyntaxError.new("Base-level rules cannot contain the parent-selector-referencing character '#{PARENT}'.", line)
      else
        per_rule_indent, total_indent = [:nested, :expanded].include?(@style) ? [rule_indent, ''] : ['', rule_indent]
        total_indent + self.rules.map do |r|
          per_rule_indent + r.gsub(/,$/, '').gsub(rule_split, rule_separator).rstrip
        end.join(line_separator)
      end

      children.each do |child|
        if child.is_a? RuleNode
          sub_rules << child
        else
          attributes << child
        end
      end

      to_return = ''
      if !attributes.empty?
        old_spaces = '  ' * (tabs - 1)
        spaces = '  ' * tabs
        if @style == :compact
          attributes = attributes.map { |a| a.to_s(1) }.join(' ')
          to_return << "#{total_rule} { #{attributes} }\n"
        elsif @style == :compressed
          attributes = attributes.map { |a| a.to_s(1) }.join(';')
          to_return << "#{total_rule}{#{attributes}}"
        else
          attributes = attributes.map { |a| a.to_s(tabs + 1) }.join("\n")
          end_attrs = (@style == :expanded ? "\n" + old_spaces : ' ')
          to_return << "#{total_rule} {\n#{attributes}#{end_attrs}}\n"
        end
      end

      tabs += 1 unless attributes.empty? || @style != :nested
      sub_rules.each do |sub|
        to_return << sub.to_s(tabs, total_rule)
      end

      to_return
    end
  end
end
