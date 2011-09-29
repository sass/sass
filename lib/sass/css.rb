require File.dirname(__FILE__) + '/../sass'
require 'sass/tree/node'
require 'sass/scss/css_parser'
require 'strscan'

module Sass
  # This class converts CSS documents into Sass or SCSS templates.
  # It works by parsing the CSS document into a {Sass::Tree} structure,
  # and then applying various transformations to the structure
  # to produce more concise and idiomatic Sass/SCSS.
  #
  # Example usage:
  #
  #     Sass::CSS.new("p { color: blue }").render(:sass) #=> "p\n  color: blue"
  #     Sass::CSS.new("p { color: blue }").render(:scss) #=> "p {\n  color: blue; }"
  class CSS
    # @param template [String] The CSS stylesheet.
    #   This stylesheet can be encoded using any encoding
    #   that can be converted to Unicode.
    #   If the stylesheet contains an `@charset` declaration,
    #   that overrides the Ruby encoding
    #   (see {file:SASS_REFERENCE.md#encodings the encoding documentation})
    # @option options :old [Boolean] (false)
    #     Whether or not to output old property syntax
    #     (`:color blue` as opposed to `color: blue`).
    #     This is only meaningful when generating Sass code,
    #     rather than SCSS.
    def initialize(template, options = {})
      if template.is_a? IO
        template = template.read
      end

      @options = options.dup
      # Backwards compatibility
      @options[:old] = true if @options[:alternate] == false
      @template = template
    end

    # Converts the CSS template into Sass or SCSS code.
    #
    # @param fmt [Symbol] `:sass` or `:scss`, designating the format to return.
    # @return [String] The resulting Sass or SCSS code
    # @raise [Sass::SyntaxError] if there's an error parsing the CSS template
    def render(fmt = :sass)
      check_encoding!
      build_tree.send("to_#{fmt}", @options).strip + "\n"
    rescue Sass::SyntaxError => err
      err.modify_backtrace(:filename => @options[:filename] || '(css)')
      raise err
    end

    # Returns the original encoding of the document,
    # or `nil` under Ruby 1.8.
    #
    # @return [Encoding, nil]
    # @raise [Encoding::UndefinedConversionError] if the source encoding
    #   cannot be converted to UTF-8
    # @raise [ArgumentError] if the document uses an unknown encoding with `@charset`
    def source_encoding
      check_encoding!
      @original_encoding
    end

    private

    def check_encoding!
      return if @checked_encoding
      @checked_encoding = true
      @template, @original_encoding = Sass::Util.check_sass_encoding(@template) do |msg, line|
        raise Sass::SyntaxError.new(msg, :line => line)
      end
    end

    # Parses the CSS template and applies various transformations
    #
    # @return [Tree::Node] The root node of the parsed tree
    def build_tree
      root = Sass::SCSS::CssParser.new(@template, @options[:filename]).parse
      expand_commas      root
      parent_ref_rules   root
      remove_parent_refs root
      flatten_rules      root
      fold_commas        root
      root
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
        unless child.is_a?(Tree::RuleNode) && child.rule.first.include?(',')
          expand_commas(child) if child.is_a?(Tree::DirectiveNode)
          next child
        end
        child.rule.first.split(',').map do |rule|
          next if rule.strip.empty?
          node = Tree::RuleNode.new([rule.strip])
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
      root.children.map! do |child|
        unless child.is_a?(Tree::RuleNode)
          parent_ref_rules(child) if child.is_a?(Tree::DirectiveNode)
          next child
        end

        first, rest = child.rule.first.scan(/\A(&?(?: .|[^ ])[^.#: \[]*)([.#: \[].*)?\Z/m).first

        if current_rule.nil? || current_rule.rule.first != first
          current_rule = Tree::RuleNode.new([first])
        end

        if rest
          child.rule = ["&" + rest]
          current_rule << child
        else
          current_rule.children += child.children
        end

        current_rule
      end
      root.children.compact!
      root.children.uniq!

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
        case child
        when Tree::RuleNode
          child.rule.first.gsub! /^& +/, ''
          remove_parent_refs child
        when Tree::DirectiveNode
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
      root.children.each do |child|
        case child
        when Tree::RuleNode
          flatten_rule(child)
        when Tree::DirectiveNode
          flatten_rules(child)
        end
      end
    end

    # Flattens a single rule
    #
    # @param rule [Tree::RuleNode] The candidate for flattening
    # @see #flatten_rules
    def flatten_rule(rule)
      while rule.children.size == 1 && rule.children.first.is_a?(Tree::RuleNode)
        child = rule.children.first

        if child.rule.first[0] == ?&
          rule.rule = [child.rule.first.gsub(/^&/, rule.rule.first)]
        else
          rule.rule = ["#{rule.rule.first} #{child.rule.first}"]
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
        unless child.is_a?(Tree::RuleNode)
          fold_commas(child) if child.is_a?(Tree::DirectiveNode)
          next child
        end

        if prev_rule && prev_rule.children == child.children
          prev_rule.rule.first << ", #{child.rule.first}"
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
