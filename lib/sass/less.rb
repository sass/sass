#!/usr/bin/env ruby

require 'less'

module Less
  module StyleSheet
    module Mixin4
      def build(env)
        selectors.build(env, :mixin).each do |path|
          el = path.inject(env.root) do |current, node|
            current.descend(node.selector, node) or raise MixinNameError, "#{selectors.text_value} in #{env}"
          end
          if el.is_a?(Node::Mixin::Def)
            env << Node::Mixin::Call.new(el, [], env)
          else
            sel = path.map {|e| e.sass_selector_str}.join(' ').gsub(' :', ':')
            env << Node::Mixin::Extend.new(sel)
          end
        end
      end
    end

    module Accessor1
      def build(env)
        warn <<WARNING
Sass doesn't support attribute accessors.
Ignoring #{text_value}
WARNING
        Node::Anonymous.new("/* #{text_value} */")
      end
    end

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

  module Node
    class Element
      def to_sass_tree
        if root?
          root = Sass::Tree::RootNode.new("")
          rules.each {|r| root << r.to_sass_tree}
          return root
        end

        sel = []
        el = self
        loop do
          case el.selector
          when ":", "::"
            sel << (parent.root? ? "" : "&") unless sel.last
            sel.last << el.selector << el.name
          else sel << el.selector << el.name
          end
          break unless el.rules.size == 1 && el.rules.first.is_a?(Element)
          el = el.rules.first
        end
        rule = Sass::Tree::RuleNode.new(sel.reject {|s| s.empty?}.join(" "))
        el.rules.each {|r| rule << r.to_sass_tree}
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
      class Extend
        include Entity

        def initialize(name)
          @name = name
        end

        def to_sass_tree
          Sass::Tree::ExtendNode.new([@name])
        end
      end

      class Call
        def to_sass_tree
          Sass::Tree::MixinNode.new(@mixin.name.gsub(/^\./, ''), @params.map {|v| v.to_sass_tree})
        end
      end

      class Def
        def to_sass_tree
          mixin = Sass::Tree::MixinDefNode.new(name, @params.map do |v|
              [Sass::Script::Variable.new(v), v.value.to_sass_tree]
            end)
          rules.each {|r| mixin << r.to_sass_tree}
          mixin
        end
      end
    end

    class Property
      def to_sass_tree
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
        return Sass::Script::UnaryOperation.new(_to_sass_tree(arr[1..-1]), :minus) if arr[0] == "-"

        first, rest = _sass_split(arr)
        return first if rest.empty?
        if rest[0].is_a?(Operator)
          return Sass::Script::Operation.new(first, _to_sass_tree(rest[1..-1]),
              LESS_TO_SASS_OPERATORS[rest[0]])
        end

        Sass::Script::Operation.new(first, _to_sass_tree(rest), :concat)
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
          Sass::Tree::VariableNode.new(self, @value.to_sass_tree, false)
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
end
