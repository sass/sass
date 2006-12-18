require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'sass/tree/value_node'
require 'sass/tree/rule_node'

module Sass
  # This is the class where all the parsing and processing of the Sass
  # template is done. It can be directly used by the user by creating a
  # new instance and calling <tt>render</tt> to render the template. For example:
  #
  #   template = File.load('stylesheets/sassy.sass')
  #   sass_engine = Sass::Engine.new(template)
  #   output = sass_engine.render
  #   puts output
  class Engine
    # The character that begins a CSS attribute.
    ATTRIBUTE_CHAR  = ':'[0]
    
    # The string that begins one-line comments.
    COMMENT_STRING  = '//'
  
    # Creates a new instace of Sass::Engine that will compile the given
    # template string when <tt>render</tt> is called.
    # See README for available options.
    #
    #--
    #
    # TODO: Add current options to REFRENCE.
    #
    # When adding options, remember to add information about them
    # to README!
    #++
    #
    def initialize(template, options={})
      @template = template.split("\n")
      @options = options
      @index = 0
    end
  
    # Processes the template and returns the result as a string.
    def render
      root = Tree::Node.new
      first_line, first_tabs = next_line
      build_plain_tree(first_line, root, first_tabs)
      root.to_s
    end
    
    private
    
    def build_plain_tree(line, node, tabs)
      current_tabs = tabs
      tabs ||= 0
      
      while line && (current_tabs.nil? || current_tabs >= tabs)
        if current_tabs.nil?
          line, current_tabs = next_line
        elsif current_tabs == tabs
          node << parse_line(line)
          
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
