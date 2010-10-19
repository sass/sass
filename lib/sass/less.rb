#!/usr/bin/env ruby

require 'less'

module Less
  # This is the class that Treetop defines for parsing Less files.
  # Since not everything gets parsed into the AST but is instead resolved at parse-time,
  # we need to override some of it so that it can be converted into Sass.
  module StyleSheet
    # Selector mixins that don't have arguments.
    # This depends only on the syntax at the call site;
    # if it doesn't use parens, it hits this production,
    # regardless of whether the mixin being called has arguments or not.
    module Mixin4
      def build_with_sass(env)
        selectors.build(env, :mixin).each do |path|
          el = path.inject(env.root) do |current, node|
            current.descend(node.selector, node) or raise MixinNameError, "#{selectors.text_value} in #{env}"
          end
          if el.is_a?(Node::Mixin::Def)
            # Calling a mixin with arguments, which gets compiled to a Sass mixin
            env << Node::Mixin::Call.new(el, [], env)
          else
            # Calling a mixin without arguments, which gets compiled to @extend
            sel = selector_str(path)
            base = selector_str(selector_base(path))
            if base == sel
              env << Node::SassNode.new(Sass::Tree::ExtendNode.new([sel]))
            else
              Haml::Util.haml_warn <<WARNING
WARNING: Sass doesn't support mixing in selector sequences.
Replacing "#{sel}" with "@extend #{base}"
WARNING
              env << Node::SassNode.new(Sass::Tree::CommentNode.new("// #{sel};", true))
              env << Node::SassNode.new(Sass::Tree::ExtendNode.new([base]))
            end
          end
        end
      end
      alias_method :build_without_sass, :build
      alias_method :build, :build_with_sass

      def selector_base(path)
        el, i = Haml::Util.enum_with_index(path).to_a.reverse.find {|e, i| e.selector !~ /^:{1,2}$/} ||
          [path.first, 0]
        sel = (el.selector =~ /^:{0,2}$/ ? el.selector : "")
        [Node::Element.new(el.name, sel)] + path[i+1..-1]
      end

      def selector_str(path)
        path.map {|e| e.sass_selector_str}.join(' ').gsub(' :', ':')
      end
    end

    # Property and variable declarations.
    # We want to keep track of the line number
    # so we don't space out the variables too much in the generated Sass.
    module Declaration3
      def build_with_sass(env)
        build_without_sass(env)
        env.rules.last.src_line = input.line_of(interval.first)
      end
      alias_method :build_without_sass, :build
      alias_method :build, :build_with_sass
    end

    # Comma-separated selectors.
    # Less breaks these into completely separate nodes.
    # Since we don't want this duplication in the Sass,
    # we modify the production to keep track of the original group
    # so we can reconstruct it later on.
    module Selectors2
      def build_with_sass(env, method)
        arr = build_without_sass(env, method)
        return arr if method == :mixin
        rarr = arr.map {|e| e.top(env)}
        rarr.each {|e| e.group = rarr}
        arr
      end
      alias_method :build_without_sass, :build
      alias_method :build, :build_with_sass
    end

    # Attribute accessors.
    # Sass just flat-out doesn't support these,
    # so we print a warning to that effect and compile them to comments.
    module Accessor1
      def build(env)
        Haml::Util.haml_warn <<WARNING
WARNING: Sass doesn't support attribute accessors.
Ignoring #{text_value}
WARNING
        Node::Anonymous.new("/* #{text_value} */")
      end
    end

    # @import statements.
    # Less handles these during parse-time,
    # so we want to wrap them up as a node in the tree.
    # We also include the nodes, though,
    # since we want to have access to the mixins
    # so we can tell if they take arguments or not.
    # The included nodes are hidden so they don't appear in the output.
    module Import1
      def build_with_sass(env)
        line = input.line_of(interval.first)
        import = Sass::Tree::ImportNode.new(url.value.gsub(/\.less$/, ''))
        import.line = input.line_of(interval.first)
        env << Node::SassNode.new(import)
        old_rules = env.rules.dup
        build_without_sass env
        (env.rules - old_rules).each {|r| r.hide_in_sass = true}
      rescue ImportError => e
        raise Sass::SyntaxError.new("File to import #{url.text_value} not found or unreadable", :line => line)
      end
      alias_method :build_without_sass, :build
      alias_method :build, :build_with_sass
    end

    # The IE-specific `alpha(opacity=@var)`.
    # Less manually resolves the variable here at parse-time.
    # We want to keep the variable around,
    # so we compile this to a function.
    # Less doesn't actually have an `=` operator,
    # but that's okay since it's just getting compiled to Sass anyway.
    module Entity::Alpha1
      def build(env)
        Node::Function.new("alpha",
          [Node::Expression.new([
                Node::Keyword.new("opacity"),
                Node::Operator.new("="),
                variable.build])])
      end
    end
  end

  # The Less AST classes for the document,
  # including both stylesheet-level nodes and expression-level nodes.
  # The main purpose of overriding these is to add `#to_sass_tree` functions
  # for converting to Sass.
  module Node
    module Entity
      attr_accessor :hide_in_sass
      attr_accessor :src_line
    end

    class Element
      attr_accessor :group

      def top(env)
        return self if parent.equal?(env)
        return parent.top(env)
      end

      def to_sass_tree
        if root?
          root = Sass::Tree::RootNode.new("")
          rules.each {|r| root << r.to_sass_tree}
          return root
        end
        return if hide_in_sass
        return if !self.equal?(group.first)

        last_el = nil
        sel = group.map do |el|
          comma_sel = []
          loop do
            comma_sel << el.sass_selector_str
            break unless el.rules.size == 1 && el.rules.first.is_a?(Element)
            el = el.rules.first
          end
          last_el = el
          comma_sel = comma_sel.join(' ').gsub(' :', ':')
          comma_sel.gsub!(/^:/, '&:') unless parent.root?
          comma_sel
        end.join(', ')

        rule = Sass::Tree::RuleNode.new([sel])
        last_el.rules.each {|r| rule << r.to_sass_tree}
        return rule
      end

      def sass_selector_str
        case @selector
        when /[+>~]/; "#{@selector} #{@name}"
        else @selector + @name
        end
      end
    end

    module Mixin
      class Call
        def to_sass_tree
          return if hide_in_sass
          Sass::Tree::MixinNode.new(@mixin.name.gsub(/^\./, ''), @params.map {|v| v.to_sass_tree})
        end
      end

      class Def
        def to_sass_tree
          return if hide_in_sass
          mixin = Sass::Tree::MixinDefNode.new(name, @params.map do |v|
              v.value.flatten!
              [Sass::Script::Variable.new(v), v.value.to_sass_tree]
            end)
          rules.each {|r| mixin << r.to_sass_tree}
          mixin
        end
      end
    end

    class SassNode
      include Entity

      def initialize(node)
        @node = node
      end

      def to_sass_tree
        return if hide_in_sass
        @node
      end
    end

    class Property
      def to_sass_tree
        return if hide_in_sass
        Sass::Tree::PropNode.new([self], @value.to_sass_tree, :new)
      end
    end

    class Expression
      def to_sass_tree
        if first.is_a?(Array)
          val = map {|e| _to_sass_tree(e)}.inject(nil) do |e, i|
            next i unless e
            Sass::Script::Operation.new(e, i, :comma)
          end
        else
          val = _to_sass_tree(self)
        end
        val.options = {}
        val
      end

      private

      LESS_TO_SASS_OPERATORS = {"-" => :minus, "+" => :plus, "*" => :times, "/" => :div, "=" => :single_eq}

      def _to_sass_tree(arr)
        e, rest = _to_sass_tree_plus_minus_eq(arr)
        until rest.empty?
          e2, rest = _to_sass_tree_plus_minus_eq(rest)
          e = Sass::Script::Operation.new(e, e2, :concat)
        end
        return e
      end

      def _to_sass_tree_plus_minus_eq(arr)
        e, rest = _to_sass_tree_times_div(arr)
        while rest[0] && rest[0].is_a?(Operator) && %w[+ - =].include?(rest[0])
          op = LESS_TO_SASS_OPERATORS[rest[0]]
          e2, rest = _to_sass_tree_times_div(rest[1..-1])
          e = Sass::Script::Operation.new(e, e2, op)
        end
        return e, rest
      end

      def _to_sass_tree_times_div(arr)
        e, rest = _to_sass_tree_unary(arr)
        while rest[0] && rest[0].is_a?(Operator) && %w[* /].include?(rest[0])
          op = LESS_TO_SASS_OPERATORS[rest[0]]
          e2, rest = _to_sass_tree_unary(rest[1..-1])
          e = Sass::Script::Operation.new(e, e2, op)
        end
        return e, rest
      end

      def _to_sass_tree_unary(arr)
        if arr[0] == "-"
          first, rest = _sass_split(arr[1..-1])
          return Sass::Script::UnaryOperation.new(first, :minus), rest
        else
          return _sass_split(arr[0..-1])
        end
      end

      def _sass_split(arr)
        return arr[0].to_sass_tree, arr[1..-1] unless arr[0] == "("
        parens = 1
        i = arr[1..-1].each_with_index do |e, i|
          parens += 1 if e == "("
          parens -= 1 if e == ")"
          break i if parens == 0
        end

        return _to_sass_tree(arr[1...i+1]), arr[i+2..-1]
      end
    end

    class Color
      def to_sass_tree
        Sass::Script::Color.new(:red => r, :green => g, :blue => b, :alpha => a)
      end
    end

    class Number
      def to_sass_tree
        Sass::Script::Number.new(self, [self.unit])
      end
    end

    class Variable
      def to_sass_tree
        if @declaration
          return if hide_in_sass
          node = Sass::Tree::VariableNode.new(self, @value.to_sass_tree, false)
          node.line = self.src_line
          node
        else
          Sass::Script::Variable.new(self)
        end
      end
    end

    class Function
      def to_sass_tree
        Sass::Script::Funcall.new(self, @args.map {|a| a.to_sass_tree})
      end
    end

    class Keyword
      def to_sass_tree
        Sass::Script::String.new(self)
      end
    end

    class Anonymous
      def to_sass_tree
        Sass::Script::String.new(self)
      end
    end

    class Quoted
      def to_sass_tree
        Sass::Script::String.new(self, true)
      end
    end

    class FontFamily
      def to_sass_tree
        @family.map {|f| f.to_sass_tree}.inject(nil) do |e, f|
          next f unless e
          Sass::Script::Operation.new(e, f, :comma)
        end
      end
    end
  end

  # The entry point to Less.
  # By default Less doesn't preserve the filename of the file being parsed,
  # which is unpleasant for error reporting.
  # Our monkeypatch keeps it around.
  class Engine
    def initialize_with_sass(obj, opts = {})
      initialize_without_sass(obj, opts)
      @filename = obj.path if obj.is_a?(File)
    end
    alias_method :initialize_without_sass, :initialize
    alias_method :initialize, :initialize_with_sass

    def parse_with_sass
      parse_without_sass
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:filename => @filename)
      raise e
    end
    alias_method :parse_without_sass, :parse
    alias_method :parse, :parse_with_sass
    alias_method :to_tree, :parse
  end
end
