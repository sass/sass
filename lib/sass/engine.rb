require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'sass/tree/value_node'
require 'sass/tree/rule_node'

module Sass
  class Engine
    # The character that begins a CSS attribute.
    ATTRIBUTE_CHAR  = ':'[0]
    
    # The string that begins one-line comments.
    COMMENT_STRING  = '//'
  
    def initialize(template, options={})
      @template = template.split("\n")
      @options = options
      @index = 0
    end
  
    def render
      plain_root = Tree::Node.new
      first_line, first_tabs = next_line
      
      build_plain_tree(first_line, plain_root, first_tabs)
      css_root = build_css_tree(plain_root)
      css_root.to_s
    end
    
    private
    
    def build_plain_tree(line, node, tabs)
      current_tabs = tabs
      tabs ||= 0
      
      while line && (current_tabs.nil? || current_tabs >= tabs)
        if current_tabs.nil?
          line, current_tabs = next_line
        elsif current_tabs == tabs
          node << Tree::ValueNode.new(line)
          
          line, current_tabs = next_line
        else # current_tabs > tabs        
          line, current_tabs = build_plain_tree(line, node.children[-1], current_tabs)
        end
      end
      
      [line, current_tabs]
    end
    
    def next_line
      line = @template[@index]
      return if line.nil?
      
      # TODO: Allow comments appended to the end of lines, find some way to make url(http://www.google.com/) work
      line = '' if line[0..1] == COMMENT_STRING
      current_tabs = count_soft_tabs(line)
      line.strip!
      @index += 1
      [line, current_tabs]
    end
    
    def build_css_tree(plain_node)
      if plain_node.is_a? Tree::ValueNode
        css_node = parse_line(plain_node.value)
      else
        css_node = Tree::Node.new
      end
      
      plain_node.children.each do |child|
        css_node << build_css_tree(child)
      end
      css_node
    end
    
    def parse_line(line)
      if line[0] == ATTRIBUTE_CHAR
        name, *value = line.split(' ')
        name = name[1..-1]
        Tree::AttrNode.new(name, value)
      else
        Tree::RuleNode.new(line)
      end
    end
    
    # Counts the tabulation of a line.
    def count_soft_tabs(line)
      spaces = line.index(/[^ ]/)
      spaces ? spaces/2 : nil
    end
  end
end
