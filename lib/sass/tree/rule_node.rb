require 'sass/tree/node'
require 'sass/tree/attr_node'

module Sass::Tree
  class RuleNode < ValueNode
    alias_method :rule, :value
    alias_method :rule=, :value=
    
    def to_s(super_rules = nil)
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
        to_return << "#{total_rule} { #{attributes.join(' ')} }\n"
      end
      
      sub_rules.each { |sub| to_return << sub.to_s(total_rule) }
      to_return
    end
  end
end
