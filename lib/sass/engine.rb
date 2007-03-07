require 'sass/tree/node'
require 'sass/tree/value_node'
require 'sass/tree/rule_node'
require 'sass/constant'
require 'sass/error'

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
    ATTRIBUTE_CHAR  = ?:
    
    # The character that designates that
    # an attribute should be assigned to the result of constant arithmetic.
    SCRIPT_CHAR     = ?=
    
    # The string that begins one-line comments.
    COMMENT_STRING  = '//'

    # The regex that matches attributes.
    ATTRIBUTE = /:([^\s=]+)\s*(=?)\s*(.*)/
  
    # Creates a new instace of Sass::Engine that will compile the given
    # template string when <tt>render</tt> is called.
    # See README for available options.
    #
    #--
    #
    # TODO: Add current options to REFRENCE. Remember :filename!
    #
    # When adding options, remember to add information about them
    # to README!
    #++
    #
    def initialize(template, options={})
      @options = {
        :style => :nested
      }.merge! options
      @template = template.split("\n")
      @lines = []
      @constants = {}
    end
  
    # Processes the template and returns the result as a string.
    def render
      begin
        split_lines
      
        root = Tree::Node.new(@options[:style])
        index = 0
        while @lines[index]
          child, index = build_tree(index)
          child.line = index if child
          root << child if child
        end
        @line = nil

        root.to_s
      rescue SyntaxError => err
        err.add_backtrace_entry(@options[:filename])
        raise err
      end
    end

    alias_method :to_css, :render
    
    private
    
    # Readies each line in the template for parsing,
    # and computes the tabulation of the line.
    def split_lines
      old_tabs = 0
      @template.each_with_index do |line, index|
        @line = index + 1
      
        # TODO: Allow comments appended to the end of lines,
        # find some way to make url(http://www.google.com/) work
        unless line[0..1] == COMMENT_STRING # unless line is a comment
          tabs = count_tabs(line)
          
          if tabs # if line isn't blank
            if tabs - old_tabs > 1
              raise SyntaxError.new("Illegal Indentation: Only two space characters are allowed as tabulation.", @line) 
            end
            @lines << [line.strip, tabs]

            old_tabs = tabs
          end
        end
      end
      @line = nil
    end
    
    # Counts the tabulation of a line.
    def count_tabs(line)
      spaces = line.index(/[^ ]/)
      if spaces
        if spaces % 2 == 1 || line[spaces] == ?\t
          raise SyntaxError.new("Illegal Indentation: Only two space characters are allowed as tabulation.", @line) 
        end
        spaces / 2
      else
        nil
      end
    end
    
    def build_tree(index)
      line, tabs = @lines[index]
      index += 1
      @line = index
      node = parse_line(line)

      # Node is nil if it's non-outputting, like a constant assignment
      return nil, index unless node

      has_children = has_children?(index, tabs)
      
      while has_children
        child, index = build_tree(index)

        if child.nil?
          raise SyntaxError.new("Constants may only be declared at the root of a document.", @line)
        end

        child.line = @line
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
        when Constant::CONSTANT_CHAR
          parse_constant(line)
        else
          Tree::RuleNode.new(line, @options[:style])
      end
    end
    
    def parse_attribute(line)
      name, eq, value = line.scan(ATTRIBUTE)[0]

      if name.nil? || value.nil?
        raise SyntaxError.new("Invalid attribute: \"#{line}\"", @line)
      end
      
      if eq[0] == SCRIPT_CHAR
        value = Sass::Constant.parse(value, @constants, @line).to_s
      end
      
      Tree::AttrNode.new(name, value, @options[:style])
    end
    
    def parse_constant(line)
      name, value = line.scan(Sass::Constant::MATCH)[0]
      unless name && value
        raise SyntaxError.new("Invalid constant: \"#{line}\"", @line)
      end
      @constants[name] = Sass::Constant.parse(value, @constants, @line)
      nil
    end
  end
end
