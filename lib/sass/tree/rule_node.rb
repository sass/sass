require 'sass/tree/node'
require 'sass/tree/attr_node'

module Sass::Tree
  class RuleNode < ValueNode
    alias_method :rule, :value
    alias_method :rule=, :value=
    
    def to_s(tabs, super_rules = nil)
      attributes = []
      sub_rules = []
      total_rule = if super_rules
        super_rules.split(/,\s*/).collect! do |s|
          self.rule.split(/,\s*/).collect! {|r| "#{s} #{r}"}.join(", ")
        end.join(", ")
      else
        self.rule
      end
      
      children.each do |child|
        if child.is_a? AttrNode
          attributes << child
        else
          sub_rules << child
        end
      end
      
      to_return = ''
      unless attributes.empty?
        if @style == :compact
          to_return << "#{total_rule} { #{attributes.join(' ')} }\n"
        else
          spaces = (@style == :expanded ? 2 : tabs * 2)
          old_spaces = ' ' * (spaces - 2)
          spaces = ' ' * spaces

          attributes = attributes.join("\n").gsub("\n", "\n#{spaces}").rstrip
          end_attrs = (@style == :expanded ? "\n" : ' ')
          to_return << "#{old_spaces}#{total_rule} {\n#{spaces}#{attributes}#{end_attrs}}\n"
        end
      end
      
      sub_rules.each { |sub| to_return << sub.to_s(tabs + 1, total_rule) }
      to_return
    end
  end
end
