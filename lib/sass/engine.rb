require 'sass/tree/node'
require 'sass/tree/value_node'
require 'sass/tree/rule_node'
require 'sass/constant'

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
    
    # The character that begins a constant.
    CONSTANT_CHAR   = '!'[0]
    
    # The character that designates that
    # an attribute should be assigned to the result of constant arithmetic.
    SCRIPT_CHAR     = '='[0]
    
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
      @options = options
      @template = template.split("\n")
      @lines = []
      @constants = {}
    end
  
    # Processes the template and returns the result as a string.
    def render
      split_lines
      
      root = Tree::Node.new
      index = 0
      while @lines[index]
        child, index = build_tree(index)
        root << child if child
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
      return nil, index unless node
      has_children = has_children?(index, tabs)
      
      while has_children
        child, index = build_tree(index)
        node << child if child
        has_children = has_children?(index, tabs)
      end
      
      return node, index
    end
    
    def has_children?(index, tabs)
      next_line = @lines[index]
      next_line && next_line[1] > tabs
    end
    
    def parse_line(line)
      case line[0]
        when ATTRIBUTE_CHAR
          parse_attribute(line)
        when CONSTANT_CHAR
          parse_constant(line)
        else
          Tree::RuleNode.new(line)
      end
    end
    
    def parse_attribute(line)
      name, value = line.split(' ', 2)
      name = name[1..-1]
      
      if name[-1] == SCRIPT_CHAR
        name.slice!(-1)
        value = Sass::Constant.parse(value, @constants).to_s
      end
      
      Tree::AttrNode.new(name, value)
    end
    
    def parse_constant(line)
      not_in_name = Sass::Constant::SYMBOLS.keys + [ Sass::Constant::ESCAPE_CHAR, '='[0] ]
      not_in_name.map! { |c| Regexp.escape("#{c.chr}") }
      name, value = line.scan(/^#{Regexp.escape(CONSTANT_CHAR.chr)}([^\s#{not_in_name.join}]+)\s*=\s*(.+)/)[0]
      raise "Invalid constant assignment:\n#{line}" unless name && value
      @constants[name] = Sass::Constant.parse(value, @constants)
      nil
    end
  end
end
