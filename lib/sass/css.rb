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

    class RuleNode
      def to_sass(tabs)
        str = "#{'  ' * tabs}#{rule}\n"

        children.each do |child|
          str << "#{child.to_sass(tabs + 1)}\n"
        end

        str
      end
    end

    class AttrNode
      def to_sass(tabs)
        "#{'  ' * tabs}:#{name} #{value}"
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
      build_tree.to_sass
    end

    private

    def build_tree
      root = Tree::Node.new(nil)

      while @template.scan(RULE_RE)
        rule = Tree::RuleNode.new(@template[0][0...-1].strip, nil)
        root << rule

        while @template.scan(ATTR_RE)
          attrs = @template[0][0...-1].split(':').map {|s| s.strip}
          rule << Tree::AttrNode.new(attrs[0], attrs[1], nil)
        end

        if @template.scan(/\s*\}/).nil?
          raise "Invalid CSS!"
        end
      end

      root
    end
  end
end
