require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'strscan'

module Sass
  module Tree
    class Node
      # Converts a node to Sass code that will generate it.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the Sass code
      # @param opts [Hash<Symbol, Object>] An options hash (see {Sass::CSS#initialize})
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
        str = "\n#{'  ' * tabs}#{rule}#{children.any? { |c| c.is_a? AttrNode } ? "\n" : ''}"

        children.each do |child|
          str << "#{child.to_sass(tabs + 1, opts)}"
        end

        str
      end
    end

    class AttrNode
      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{opts[:alternate] ? '' : ':'}#{name}#{opts[:alternate] ? ':' : ''} #{value}\n"
      end
    end

    class DirectiveNode
      # @see Node#to_sass
      def to_sass(tabs, opts = {})
        "#{'  ' * tabs}#{value}#{children.map {|c| c.to_sass(tabs + 1, opts)}}\n"
      end
    end
  end

  # This class is based on the Ruby 1.9 ordered hashes.
  # It keeps the semantics and most of the efficiency of normal hashes
  # while also keeping track of the order in which elements were set.
  class OrderedHash
    Node = Struct.new(:key, :value, :next, :prev)
    include Enumerable

    def initialize
      @hash = {}
    end

    # Replaces the contents of this hash with the context of `other`.
    #
    # @param [OrderedHash] other
    # @return `self`
    def initialize_copy(other)
      @hash = other.instance_variable_get('@hash').clone
      self
    end

    # Returns the value corresponding to `key`, or `nil` if not found.
    #
    # @param key
    # @return The value or `nil`
    def [](key)
      @hash[key] && @hash[key].value
    end

    # Sets the value corresponding to `key` to `value`
    #
    # @param key
    # @param value
    # @return `value`
    def []=(key, value)
      node = Node.new(key, value)

      if old = @hash[key]
        if old.prev
          old.prev.next = old.next
        else # old is @first and @last
          @first = @last = nil
        end
      end

      if @first.nil?
        @first = @last = node
      else
        node.prev = @last
        @last.next = node
        @last = node
      end

      @hash[key] = node
      value
    end

    # Yields each key-value pair in the hash
    # in the order in which they were inserted.
    #
    # @yield [key, value] The key-value pairs
    # @return `self`
    def each
      return unless @first
      yield [@first.key, @first.value]
      node = @first
      yield [node.key, node.value] while node = node.next
      self
    end

    # Returns an array of the values in the hash
    # in the order in which they were inserted.
    #
    # @return [Object] The values
    def values
      self.map {|k, v| v}
    end
  end

  # This class converts CSS documents into Sass templates.
  # It works by parsing the CSS document into a {Sass::Tree} structure,
  # and then applying various transformations to the structure
  # to produce more concise and idiomatic Sass.
  #
  # Example usage:
  #
  #     Sass::CSS.new("p { color: blue }").render #=> "p\n  :color blue"
  class CSS
    # @param [String] template The CSS code
    # @param [Hash<Symbol, Object>] options An options hash.
    #   `:alternate`: Whether or not to output alternate attribute syntax
    #   (`color: blue` as opposed to `:color blue`).
    def initialize(template, options = {})
      if template.is_a? IO
        template = template.read
      end

      @options = options
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
    # @param [Tree::Node] The parent node of the rules
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

    # Parses a set of CSS attributes within a rule.
    #
    # @param [Tree::RuleNode] rule The parent node of the attributes
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
    # @param [Regexp] The regular expression to assert
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
    # @param [Tree::Node] root The parent node
    def expand_commas(root)
      root.children.map! do |child|
        next child unless Tree::RuleNode === child && child.rule.include?(',')
        child.rule.split(',').map do |rule|
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
    # @param [Tree::Node] root The parent node
    def parent_ref_rules(root)
      rules = OrderedHash.new
      root.children.select { |c| Tree::RuleNode === c }.each do |child|
        root.children.delete child
        first, rest = child.rule.scan(/^(&?(?: .|[^ ])[^.#: \[]*)([.#: \[].*)?$/).first
        rules[first] ||= Tree::RuleNode.new(first)
        if rest
          child.rule = "&" + rest
          rules[first] << child
        else
          rules[first].children += child.children
        end
      end

      rules.values.each { |v| parent_ref_rules(v) }
      root.children += rules.values
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
    # @param [Tree::Node] root The parent node
    def remove_parent_refs(root)
      root.children.each do |child|
        if child.is_a?(Tree::RuleNode)
          child.rule.gsub! /^& +/, ''
          remove_parent_refs child
        end
      end
    end

    # Flatten rules so that
    #
    #     foo
    #       bar
    #         :color red
    #
    # becomes
    #
    #     foo bar
    #       :color red
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
    # @param [Tree::Node] root The parent node
    def flatten_rules(root)
      root.children.each { |child| flatten_rule(child) if child.is_a?(Tree::RuleNode) }
    end

    # Flattens a single rule
    #
    # @param [Tree::RuleNode] rule The candidate for flattening
    # @see #flatten_rules
    def flatten_rule(rule)
      while rule.children.size == 1 && rule.children.first.is_a?(Tree::RuleNode)
        child = rule.children.first

        if child.rule[0] == ?&
          rule.rule = child.rule.gsub /^&/, rule.rule
        else
          rule.rule = "#{rule.rule} #{child.rule}"
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
    # @param [Tree::RuleNode] rule The candidate for flattening
    def fold_commas(root)
      prev_rule = nil
      root.children.map! do |child|
        next child unless child.is_a?(Tree::RuleNode)

        if prev_rule && prev_rule.children == child.children
          prev_rule.rule << ", #{child.rule}"
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
