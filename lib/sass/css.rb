require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'strscan'

module Sass
  # :stopdoc:
  module Tree
    class Node
      def to_sass
        result = ''

        children.each do |child|
          result << "#{child.to_sass(0)}\n"
        end

        result
      end
    end

    class ValueNode
      def to_sass(tabs)
        "#{value}\n"
      end
    end

    class RuleNode
      def to_sass(tabs)
        str = "#{'  ' * tabs}#{rule}\n"

        children.each do |child|
          str << "#{child.to_sass(tabs + 1)}"
        end

        str
      end
    end

    class AttrNode
      def to_sass(tabs)
        "#{'  ' * tabs}:#{name} #{value}\n"
      end
    end
  end
  # :startdoc:

  # This class contains the functionality used in the +css2sass+ utility,
  # namely converting CSS documents to Sass templates.
  class CSS
    # :stopdoc:

    # The Regexp matching a CSS rule
    RULE_RE = /\s*([^\{]+)\s*\{/

    # The Regexp matching a CSS attribute
    ATTR_RE = /\s*[^::\{\}]+\s*:\s*[^:;\{\}]+\s*;/

    # :startdoc:

    # Creates a new instance of Sass::CSS that will compile the given document
    # to a Sass string when +render+ is called.
    def initialize(template)
      if template.is_a? IO
        template = template.read
      end

      @template = StringScanner.new(template)
    end

    # Processes the document and returns the result as a string
    # containing the CSS template.
    def render
      begin
        build_tree.to_sass
      rescue Exception => err
        line = @template.string[0...@template.pos].split("\n").size
        
        err.backtrace.unshift "(css):#{line}"
        raise err
      end
    end

    private

    def build_tree
      root = Tree::Node.new(nil)
      whitespace
      directives(root)
      rules(root)
      sort_rules(root)
      root
    end

    def directives(root)
      while @template.scan(/@/)
        name = @template.scan /[^\s;]+/
        whitespace
        value = @template.scan /[^;]+/
        assert_match /;/
        whitespace

        if name == "import" && value =~ /^(url\()?"?([^\s\(\)\"]+)\.css"?\)?$/
          value = $2
        end

        root << Tree::ValueNode.new("@#{name} #{value};", nil)
      end
    end

    def rules(root)
      rules = []
      while @template.scan(/[^\{\s]+/)
        rules << @template[0]
        whitespace

        if @template.scan(/\{/)
          result = Tree::RuleNode.new(rules.join(' '), nil)
          root << result
          rules = []

          whitespace
          attributes(result)
        end
      end
    end

    def attributes(rule)
      while @template.scan(/[^:\}\s]+/)
        name = @template[0]
        whitespace

        assert_match /:/
        
        value = ''
        while @template.scan(/[^;\s]+/)
          value << @template[0] << whitespace
        end
        
        assert_match /;/        
        rule << Tree::AttrNode.new(name, value, nil)
      end

      assert_match /\}/
    end

    def whitespace
      space = @template.scan(/\s*/) || ''

      # If we've hit a comment,
      # go past it and look for more whitespace
      if @template.scan(/\/\*/)
        @template.scan_until(/\*\//)
        return space + whitespace
      end
      return space
    end

    def assert_match(re)
      if !@template.scan(re)
        raise Exception.new("Invalid CSS!")
      end
      whitespace
    end

    def sort_rules(root)
      root.children.sort! do |c1, c2|
        if c1.is_a?(Tree::RuleNode) && c2.is_a?(Tree::RuleNode)
          c1.rule <=> c2.rule
        elsif !(c1.is_a?(Tree::RuleNode) || c2.is_a?(Tree::RuleNode)) || c2.is_a?(Tree::RuleNode)
          -1
        else
          1
        end
      end

      prev_rules = []
      prev_rule_values = []
      root.children.each do |child|
        if child.is_a? Tree::RuleNode
          joined_prev_values = prev_rule_values.join(' ')
          until prev_rules.empty? || child.rule =~ /^#{Regexp.escape(joined_prev_values)}/
            prev_rules.pop
            prev_rule_values.pop
          end
          
          unless prev_rules.empty?
            child.rule.slice!(0..(joined_prev_values.size))
            prev_rules[-1] << child
            root.children.delete child
          end
          
          prev_rules << child
          prev_rule_values << child.rule
        end
      end
    end
  end
end
