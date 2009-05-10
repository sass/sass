require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'strscan'

module Sass
  # :stopdoc:
  module Tree
    class Node
      def to_sass(opts = {})
        result = ''

        children.each do |child|
          result << "#{child.to_sass(0, opts)}\n"
        end

        result
      end
    end

    class RuleNode
      def to_sass(tabs, opts = {})
        str = "\n#{'  ' * tabs}#{rules.first}#{children.any? { |c| c.is_a? AttrNode } ? "\n" : ''}"

        children.each do |child|
          str << "#{child.to_sass(tabs + 1, opts)}"
        end

        str
      end
    end

    class AttrNode
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{opts[:alternate] ? '' : ':'}#{name}#{opts[:alternate] ? ':' : ''} #{value}\n"
      end
    end

    class DirectiveNode
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{value}#{children.map {|c| c.to_sass(tabs + 1, opts)}}\n"
      end
    end
  end

  # :startdoc:

  # This class contains the functionality used in the +css2sass+ utility,
  # namely converting CSS documents to Sass templates.
  class CSS

    # Creates a new instance of Sass::CSS that will compile the given document
    # to a Sass string when +render+ is called.
    def initialize(template, options = {})
      if template.is_a? IO
        template = template.read
      end

      @options = options
      @template = StringScanner.new(template)
    end

    # Processes the document and returns the result as a string
    # containing the CSS template.
    def render
      begin
        build_tree.to_sass(@options).strip + "\n"
      rescue Exception => err
        line = @template.string[0...@template.pos].split("\n").size

        err.backtrace.unshift "(css):#{line}"
        raise err
      end
    end

    private

    def build_tree
      root = Tree::Node.new
      whitespace
      rules              root
      expand_commas      root
      parent_ref_rules   root
      remove_parent_refs root
      flatten_rules      root
      fold_commas        root
      root
    end

    def rules(root)
      while r = rule
        root << r
        whitespace
      end
    end

    def rule
      return unless rule = @template.scan(/[^\{\};]+/)
      rule.strip!
      directive = rule[0] == ?@

      if directive
        node = Tree::DirectiveNode.new(rule)
        return node if @template.scan(/;/)

        assert_match /\{/
        whitespace

        rules(node)
        return node
      end

      assert_match /\{/
      node = Tree::RuleNode.new(rule)
      attributes(node)
      return node
    end

    def attributes(rule)
      while @template.scan(/[^:\}\s]+/)
        name = @template[0]
        whitespace

        assert_match /:/

        value = ''
        while @template.scan(/[^;\s\}]+/)
          value << @template[0] << whitespace
        end

        assert_match /(;|(?=\}))/
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
        line = @template.string[0..@template.pos].count "\n"
        # Display basic regexps as plain old strings
        expected = re.source == Regexp.escape(re.source) ? "\"#{re.source}\"" : re.inspect
        raise Exception.new("Invalid CSS on line #{line}: expected #{expected}")
      end
      whitespace
    end

    # Transform
    #
    #   foo, bar, baz
    #     color: blue
    #
    # into
    #
    #   foo
    #     color: blue
    #   bar
    #     color: blue
    #   baz
    #     color: blue
    #
    # Yes, this expands the amount of code,
    # but it's necessary to get nesting to work properly.
    def expand_commas(root)
      root.children.map! do |child|
        next child unless Tree::RuleNode === child && child.rules.first.include?(',')
        child.rules.first.split(',').map do |rule|
          node = Tree::RuleNode.new(rule.strip)
          node.children = child.children
          node
        end
      end
      root.children.flatten!
    end

    # Make rules use parent refs so that
    #
    #   foo
    #     color: green
    #   foo.bar
    #     color: blue
    #
    # becomes
    #
    #   foo
    #     color: green
    #     &.bar
    #       color: blue
    #
    # This has the side effect of nesting rules,
    # so that
    #
    #   foo
    #     color: green
    #   foo bar
    #     color: red
    #   foo baz
    #     color: blue
    #
    # becomes
    #
    #   foo
    #     color: green
    #     & bar
    #       color: red
    #     & baz
    #       color: blue
    #
    def parent_ref_rules(root)
      current_rule = nil
      root.children.select { |c| Tree::RuleNode === c }.each do |child|
        root.children.delete child
        first, rest = child.rules.first.scan(/^(&?(?: .|[^ ])[^.#: \[]*)([.#: \[].*)?$/).first

        if current_rule.nil? || current_rule.rules.first != first
          current_rule = Tree::RuleNode.new(first)
          root << current_rule
        end

        if rest
          child.rules = ["&" + rest]
          current_rule << child
        else
          current_rule.children += child.children
        end
      end

      root.children.each { |v| parent_ref_rules(v) }
    end

    # Remove useless parent refs so that
    #
    #   foo
    #     & bar
    #       color: blue
    #
    # becomes
    #
    #   foo
    #     bar
    #       color: blue
    #
    def remove_parent_refs(root)
      root.children.each do |child|
        if child.is_a?(Tree::RuleNode)
          child.rules.first.gsub! /^& +/, ''
          remove_parent_refs child
        end
      end
    end

    # Flatten rules so that
    #
    #   foo
    #     bar
    #       baz
    #         color: red
    #
    # becomes
    #
    #   foo bar baz
    #     color: red
    #
    # and
    #
    #   foo
    #     &.bar
    #       color: blue
    #
    # becomes
    #
    #   foo.bar
    #     color: blue
    #
    def flatten_rules(root)
      root.children.each { |child| flatten_rule(child) if child.is_a?(Tree::RuleNode) }
    end

    def flatten_rule(rule)
      while rule.children.size == 1 && rule.children.first.is_a?(Tree::RuleNode)
        child = rule.children.first

        if child.rules.first[0] == ?&
          rule.rules = [child.rules.first.gsub(/^&/, rule.rules.first)]
        else
          rule.rules = ["#{rule.rules.first} #{child.rules.first}"]
        end

        rule.children = child.children
      end

      flatten_rules(rule)
    end

    # Transform
    #
    #   foo
    #     bar
    #       color: blue
    #     baz
    #       color: blue
    #
    # into
    #
    #   foo
    #     bar, baz
    #       color: blue
    #
    def fold_commas(root)
      prev_rule = nil
      root.children.map! do |child|
        next child unless child.is_a?(Tree::RuleNode)

        if prev_rule && prev_rule.children == child.children
          prev_rule.rules.first << ", #{child.rules.first}"
          next nil
        end

        fold_commas(child)
        prev_rule = child
        child
      end
      root.children.compact!
    end
  end
end
