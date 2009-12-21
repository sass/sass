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
          @expected = "string or url()"
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
          @expected = "string or url()"
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
        # Many of these operators (all except / and ,)
        # are disallowed by the CSS spec,
        # but they're included here for compatibility
        # with some proprietary MS properties
        ss if raw('/') || raw(',') || raw(':') || raw('.') || raw('=')
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
        # The combinator here allows the "> E" hack
        return unless combinator || simple_selector_sequence
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

        # The raw('*') allows the "E*" hack
        nil while tok(:hash) || class_expr || attrib ||
          negation || pseudo || raw('*')
        true
      end

      def class_expr
        return unless raw '.'
        tok! :ident
      end

      def element_name
        res = tok(:ident) || raw('*')
        if raw '|'
          @expected = "element name or *"
          res = tok(:ident) || raw!('*')
        end
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
          @expected = "identifier or string"
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

        @expected = "pseudoclass or pseudoelement"
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
        @expected = "selector"
        element_name || tok(:hash) || class_expr || attrib || expr!(:pseudo)
      end

      def declaration
        # The raw('*') allows the "*prop: val" hack
        if raw '*'
          name = expr!(:property)
        else
          return unless name = property
        end

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
        unless tok(:number) ||
            tok(:uri) ||
            function ||
            tok(:string) ||
            tok(:ident) ||
            tok(:unicoderange) ||
            hexcolor
          return unless unary_operator
          @expected = "number or function"
          tok(:number) || expr!(:function)
        end
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

      EXPR_NAMES = {
        :medium => "medium (e.g. print, screen)",
        :pseudo_expr => "expression (e.g. fr, 2n+1)",
        :expr => "expression (e.g. 1px, bold)",
      }

      TOK_NAMES = {
        :ident => "identifier",
      }

      def expr!(name)
        (e = send(name)) && (return e)
        expected(EXPR_NAMES[name] || name.to_s)
      end

      def tok!(name)
        (t = tok(name)) && (return t)
        expected(TOK_NAMES[name] || name.to_s)
      end

      def raw!(chr)
        return true if raw(chr)
        expected(chr.inspect)
      end

      def expected(name)
        pos = @scanner.pos
        line = @scanner.string[0...pos].count("\n") + 1

        after = @scanner.string[[pos - 15, 0].max...pos].gsub(/.*\n/m, '')
        after = "..." + after if pos >= 15

        expected = @expected || name

        was = @scanner.rest[0...15].gsub(/\n.*/m, '')
        was += "..." if @scanner.rest.size >= 15
        raise Sass::SyntaxError.new(
          "Invalid CSS after #{after.inspect}: expected #{expected}, was #{was.inspect}",
          :line => line)
      end

      def tok(name)
        res = @scanner.scan(RX.const_get(name.to_s.upcase))
        @expected = nil if res
        @str << res if res && @str && name != :comment
        res
      end

      def raw(chr)
        res = @scanner.scan(RX.quote(chr))
        @expected = nil if res
        @str << res if res && @str
        res
      end
    end
  end
end
