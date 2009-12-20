require 'sass/scss/rx'
require 'sass/css'

require 'strscan'

module Sass
  module SCSS
    class Parser
      def initialize(str)
        @scanner = StringScanner.new(str)
      end

      def parse
        root = stylesheet
        raise "Invalid CSS at #{@scanner.rest.inspect}" unless @scanner.eos?
        root
      end

      private

      def stylesheet
        root = Sass::Tree::RootNode.new(@scanner.string)

        if tok :charset
          ss
          root << Sass::Tree::DirectiveNode.new("@charset #{tok!(:string).strip}")
          ss
          raw! ';'
        end

        s

        while child = import || namespace || ruleset || media ||
            page || font_face
          root << child
          s
        end
        root
      end

      def s
        nil while tok(:s) || tok(:cdc) || tok(:cdo) || tok(:comment)
        true
      end

      def ss
        nil while tok(:s) || tok(:comment)
        true
      end

      def import
        return unless tok(:import)
        ss
        val = str do
          tok(:string) || tok!(:uri); ss
          if medium
            while raw ','
              ss; expr! :medium
            end
          end
        end
        raw! ';'; ss
        Sass::Tree::DirectiveNode.new("@import #{val.strip}")
      end

      def namespace
        return unless tok(:namespace)
        ss
        val = str do
          ss if namespace_prefix
          tok(:string) || tok!(:uri); ss
        end
        raw! ';'; ss
        Sass::Tree::DirectiveNode.new("@namespace #{val.strip}")
      end

      def namespace_prefix
        tok :ident
      end

      def media
        return unless tok :media
        ss
        val = str do
          expr! :medium
          while raw ','
            ss; expr! :medium
          end
        end
        node = Sass::Tree::DirectiveNode.new("@media #{val.strip}")

        raw! '{'; ss
        while child = ruleset
          node << child
        end
        raw! '}'; ss
        node
      end

      def medium
        return unless tok :ident
        ss
      end

      def page
        return unless tok :page
        ss
        val = str do
          tok :ident
          pseudo_page; ss
        end
        declarations(Sass::Tree::DirectiveNode.new("@page #{val.strip}"))
      end

      def pseudo_page
        return unless raw ':'
        tok! :ident
      end

      def font_face
        return unless tok :font_face
        ss
        declarations(Sass::Tree::DirectiveNode.new("@font-face"))
      end

      def operator
        ss if raw('/') || raw(',')
        true
      end

      def unary_operator
        raw('-') || raw('+')
      end

      def property
        return unless name = tok(:ident)
        ss
        name
      end

      def ruleset
        rules = str do
          return unless selector
          while raw ','
            ss; expr!(:selector)
          end
        end
        declarations(Sass::Tree::RuleNode.new(rules.strip))
      end

      def declarations(node)
        raw! '{'; ss
        node << declaration
        while raw ';'
          ss
          node << declaration
        end
        raw! '}'; ss
        node
      end

      def selector
        return unless simple_selector_sequence
        simple_selector_sequence while combinator
        true
      end

      def combinator
        tok(:plus) || tok(:greater) || tok(:tilde) || tok(:s)
      end

      def simple_selector_sequence
        unless element_name || tok(:hash) || class_expr ||
            attrib || negation || pseudo
          # This allows for stuff like http://www.w3.org/TR/css3-animations/#keyframes-
          return expr
        end

        nil while tok(:hash) || class_expr || attrib || negation || pseudo
        true
      end

      def class_expr
        return unless raw '.'
        tok! :ident
      end

      def element_name
        res = tok(:ident) || raw('*')
        res = tok(:ident) || raw!('*') if raw '|'
        res
      end

      def attrib
        return unless raw('[')
        ss
        attrib_name!; ss
        if raw('=') ||
            tok(:includes) ||
            tok(:dashmatch) ||
            tok(:prefixmatch) ||
            tok(:suffixmatch) ||
            tok(:substringmatch)
          ss
          tok(:ident) || tok!(:string); ss
        end
        raw! ']'
      end

      def attrib_name!
        if tok(:ident)
          # E or E|E
          tok! :ident if raw('|')
        elsif raw('*')
          # *|E
          raw! '|'
          tok! :ident
        else
          # |E or E
          raw '|'
          tok! :ident
        end
      end

      def pseudo
        return unless raw ':'
        raw ':'

        functional_pseudo || tok!(:ident)
      end

      def functional_pseudo
        return unless tok :function
        ss
        expr! :pseudo_expr
        raw! ')'
      end

      def pseudo_expr
        return unless tok(:plus) || raw('-') || tok(:number) ||
          tok(:string) || tok(:ident)
        ss
        ss while tok(:plus) || raw('-') || tok(:number) ||
          tok(:string) || tok(:ident)
        true
      end

      def negation
        return unless tok(:not)
        ss
        element_name || tok(:hash) || class_expr || attrib || expr!(:pseudo)
      end

      def declaration
        return unless name = property
        raw! ':'; ss
        value = str do
          expr! :expr
          prio
        end
        Sass::Tree::PropNode.new(name, value.strip, :new)
      end

      def prio
        return unless tok :important
        ss
      end

      def expr
        return unless term
        nil while operator && term
        true
      end

      def term
        if unary_operator
          tok(:number) || expr!(:function)
          ss
          return true
        end

        return unless tok(:number) ||
          tok(:uri) ||
          function ||
          tok(:string) ||
          tok(:ident) ||
          tok(:unicoderange) ||
          hexcolor
        ss
      end

      def function
        return unless tok :function
        ss
        expr! :expr
        raw! ')'; ss
      end

      #  There is a constraint on the color that it must
      #  have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
      #  after the "#"; e.g., "#000" is OK, but "#abcd" is not.
      def hexcolor
        return unless tok :hash
        ss
      end

      def str
        @str = ""
        yield
        @str
      ensure
        @str = nil
      end

      def expr!(name)
        (e = send(name)) && (return e)
        raise "Expected #{name} expression, was #{@scanner.rest.inspect}"
      end

      def tok!(name)
        (t = tok(name)) && (return t)
        raise "Expected #{name} token at #{@scanner.rest.inspect}"
      end

      def raw!(chr)
        return true if raw(chr)
        raise "Expected #{chr.inspect} at #{@scanner.rest.inspect}"
      end

      def tok(name)
        res = @scanner.scan(RX.const_get(name.to_s.upcase))
        @str << res if res && @str && name != :comment
        res
      end

      def raw(chr)
        res = @scanner.scan(RX.quote(chr))
        @str << res if res && @str
        res
      end
    end
  end
end
