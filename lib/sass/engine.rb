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
      @lines = []
      @options = options
    end
  
    # Processes the template and returns the result as a string.
    def render
      split_lines
      
      root = Tree::Node.new
      index = 0
      while @lines[index]
        child, index = build_tree(index)
        root << child
      end
      root.to_s
    end
    
    private
    
    # Readies each line in the template for parsing,
    # and computes the tabulation of the line.
    def split_lines
      @template.each do |line|
      
        # TODO: Allow comments appended to the end of lines, find some way to make url(http://www.google.com/) work
        unless line[0..1] == COMMENT_STRING # unless line is a comment
          tabs = count_tabs(line)
          
          if tabs # if line isn't blank
            @lines << [line.strip, tabs]
          end
        end
      end
    end
    
    # Counts the tabulation of a line.
    def count_tabs(line)
      spaces = line.index(/[^ ]/)
      spaces ? spaces/2 : nil
    end
    
    def build_tree(index)
      line, tabs = @lines[index]
      index += 1
      node = parse_line(line)
      has_children = has_children?(index, tabs)
      
      while has_children
        child, index = build_tree(index)
        node << child
        has_children = has_children?(index, tabs)
      end
      
      return node, index
    end
    
    def has_children?(index, tabs)
      next_line = @lines[index]
      next_line && next_line[1] > tabs
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
  end
end
