require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'strscan'

module Sass
  module Tree
    class Node
      # Converts a node to Sass code that will generate it.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the Sass code
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @return [String] The Sass code corresponding to the node
      def to_sass(tabs = 0, opts = {})
        result = ''

        children.each do |child|
          result << "#{'  ' * tabs}#{child.to_sass(0, opts)}\n"
        end

        result
      end
    end

    class RuleNode
      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        name = rules.first
        name = "\\" + name if name[0] == ?:
        str = "\n#{'  ' * tabs}#{name}#{children.any? { |c| c.is_a? PropNode } ? "\n" : ''}"

        children.each do |child|
          str << "#{child.to_sass(tabs + 1, opts)}"
        end

        str
      end
    end

    class PropNode
      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{opts[:old] ? ':' : ''}#{name}#{opts[:old] ? '' : ':'} #{value}\n"
      end
    end

    class DirectiveNode
      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{value}#{children.map {|c| c.to_sass(tabs + 1, opts)}}\n"
      end
    end
  end

  # This class converts CSS documents into Sass templates.
  # It works by parsing the CSS document into a {Sass::Tree} structure,
  # and then applying various transformations to the structure
  # to produce more concise and idiomatic Sass.
  #
  # Example usage:
  #
  #     Sass::CSS.new("p { color: blue }").render #=> "p\n  color: blue"
  class CSS
    # @param template [String] The CSS code
    # @option options :old [Boolean] (false)
    #     Whether or not to output old property syntax
    #     (`:color blue` as opposed to `color: blue`).
    def initialize(template, options = {})
      if template.is_a? IO
        template = template.read
      end

      @options = options.dup
      # Backwards compatibility
      @options[:old] = true if @options[:alternate] == false
      @template = StringScanner.new(template)
    end

    # Converts the CSS template into Sass code.
    #
    # @return [String] The resulting Sass code
    def render
      begin
        build_tree.to_sass(0, @options).strip + "\n"
      rescue Exception => err
        line = @template.string[0...@template.pos].split("\n").size

        err.backtrace.unshift "(css):#{line}"
        raise err
      end
    end

    private

    # Parses the CSS template and applies various transformations
    #
    # @return [Tree::Node] The root node of the parsed tree
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

    # Parses a set of CSS rules.
    #
    # @param root [Tree::Node] The parent node of the rules
    def rules(root)
      while r = rule
        root << r
        whitespace
      end
    end

    # Parses a single CSS rule.
    #
    # @return [Tree::Node] The parsed rule
    def rule
      rule = ""
      loop do
        token = @template.scan(/(?:[^\{\};\/\s]|\/[^*])+/)
        if token.nil?
          return if rule.empty?
          break
        end
        rule << token
        break unless @template.match?(/\s|\/\*/)
        whitespace
        rule << " "
      end

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
      properties(node)
      return node
    end

    # Parses a set of CSS properties within a rule.
    #
    # @param rule [Tree::RuleNode] The parent node of the properties
    def properties(rule)
      while @template.scan(/[^:\}\s]+/)
        name = @template[0]
        whitespace

        assert_match /:/

        value = ''
        while @template.scan(/[^;\s\}]+/)
          value << @template[0] << whitespace
        end

        assert_match /(;|(?=\}))/
        rule << Tree::PropNode.new(name, value, nil)
      end

      assert_match /\}/
    end

    # Moves the scanner over a section of whitespace or comments.
    #
    # @return [String] The ignored whitespace
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

    # Moves the scanner over a regular expression,
    # raising an exception if it doesn't match.
    #
    # @param re [Regexp] The regular expression to assert
    def assert_match(re)
      if @template.scan(re)
        whitespace
        return
      end

      line = @template.string[0..@template.pos].count "\n"
      pos = @template.pos

      after = @template.string[pos - 15...pos]
      after = "..." + after if pos >= 15

      # Display basic regexps as plain old strings
      expected = re.source == Regexp.escape(re.source) ? "\"#{re.source}\"" : re.inspect

      was = @template.rest[0...15]
      was += "..." if @template.rest.size >= 15
      raise Exception.new(<<MESSAGE)
Invalid CSS on line #{line + 1} after #{after.inspect}:
  expected #{expected}, was #{was.inspect}
MESSAGE
    end

    # Transform
    #
    #     foo, bar, baz
    #       color: blue
    #
    # into
    #
    #     foo
    #       color: blue
    #     bar
    #       color: blue
    #     baz
    #       color: blue
    #
    # @param root [Tree::Node] The parent node
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
    #     foo
    #       color: green
    #     foo.bar
    #       color: blue
    #
    # becomes
    #
    #     foo
    #       color: green
    #       &.bar
    #         color: blue
    #
    # This has the side effect of nesting rules,
    # so that
    #
    #     foo
    #       color: green
    #     foo bar
    #       color: red
    #     foo baz
    #       color: blue
    #
    # becomes
    #
    #     foo
    #       color: green
    #       & bar
    #         color: red
    #       & baz
    #         color: blue
    #
    # @param root [Tree::Node] The parent node
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
    #     foo
    #       & bar
    #         color: blue
    #
    # becomes
    #
    #     foo
    #       bar
    #         color: blue
    #
    # @param root [Tree::Node] The parent node
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
    #     foo
    #       bar
    #         color: red
    #
    # becomes
    #
    #     foo bar
    #       color: red
    #
    # and
    #
    #     foo
    #       &.bar
    #         color: blue
    #
    # becomes
    #
    #     foo.bar
    #       color: blue
    #
    # @param root [Tree::Node] The parent node
    def flatten_rules(root)
      root.children.each { |child| flatten_rule(child) if child.is_a?(Tree::RuleNode) }
    end

    # Flattens a single rule
    #
    # @param rule [Tree::RuleNode] The candidate for flattening
    # @see #flatten_rules
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
    #     foo
    #       bar
    #         color: blue
    #       baz
    #         color: blue
    #
    # into
    #
    #     foo
    #       bar, baz
    #         color: blue
    #
    # @param rule [Tree::RuleNode] The candidate for flattening
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
