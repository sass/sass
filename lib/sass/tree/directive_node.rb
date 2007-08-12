require 'sass/tree/node'
require 'sass/tree/value_node'

module Sass::Tree
  class DirectiveNode < ValueNode
    def to_s(tabs)
      if children.empty?
        value + ";"
      else
        result = "#{'  ' * (tabs - 1)}#{value} {" + (@style == :compact ? ' ' : "\n")
        was_attr = false
        first = true
        children.each do |child|
          if child.is_a?(RuleNode) && child.continued?
            check_multiline_rule(child)
            continued_rule = true
          end

          if @style == :compact
            if child.is_a?(AttrNode)
              result << "#{child.to_s(first || was_attr ? 1 : tabs + 1)} "
            else
              if was_attr
                result[-1] = "\n"
              end
              rendered = child.to_s(tabs + 1)
              rendered.lstrip! if first
              result << rendered
            end
            was_attr = child.is_a?(AttrNode)
            first = continued_rule
          else
            result << child.to_s(tabs + 1) + (continued_rule ? '' : "\n")
          end
        end
        result.rstrip + (@style == :expanded ? "\n" : " ") + "}\n"
      end
    end
  end
end
