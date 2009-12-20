require 'strscan'
require 'rx'

module Sass
  module SCSS
    class Parser
      def initialize(str)
        @scanner = StringScanner.new(str)
      end

      def parse
        stylesheet
        raise "Invalid CSS!" unless @scanner.eos?
      end

      private

      def stylesheet
        if tok :charset
          ss
          tok! :string; ss
          raw! ';'
        end

        s

        s while import
        s while namespace
        s while ruleset || media || page || font_face
        true
      end

      def s
        nil while tok(:s) || tok(:cdc) || tok(:cdo)
        true
      end

      def ss
        nil while tok :s
        true
      end

      def import
        return unless tok(:import)
        ss
        tok(:string) || tok!(:uri); ss
        if medium
          while raw ','
            ss; expr! :medium
          end
        end
        raw! ';'; ss
      end

      def namespace
        return unless tok(:namespace)
        ss
        ss if namespace_prefix
        tok(:string) || tok!(:uri); ss
        raw! ';'; ss
      end

      def namespace_prefix
        tok :ident
      end

      def media
        return unless tok :media
        ss
        expr! :medium
        while raw ','
          ss; expr! :medium
        end

        raw! '{'; ss
        nil while ruleset
        raw! '}'; ss
      end

      def medium
        return unless tok :ident
        ss
      end

      def page
        return unless tok :page
        ss
        tok :ident
        pseudo_page; ss
        raw! '{'; ss
        expr! :declaration
        while raw ';'
          ss; expr! :declaration
        end
        raw! '}'; ss
      end

      def pseudo_page
        return unless raw ':'
        tok! :ident
      end

      def font_face
        return unless tok :font_face
        ss
        raw! '{'; ss
        expr! :declaration
        while raw ';'
          ss; expr! :declaration
        end
        raw! '}'; ss
      end

      def operator
        ss if raw('/') || raw(',')
        true
      end

      def combinator
        ss if raw('+') || raw('>')
        true
      end

      def unary_operator
        raw('-') || raw('+')
      end

      def property
        return unless tok :ident
        ss
      end

      def ruleset
        return unless selector
        while raw ','
          ss; expr! :selector
        end
        raw! '{'; ss
        expr! :declaration
        while raw ';'
          ss; expr! :declaration
        end
        raw! '}'; ss
      end

      def selector
        res = simple_selector
        while add = combinator && simple_selector
          res ||= add
        end
        res
      end

      def simple_selector
        res = element_name
        while mod = tok(:hash) || class_expr || attrib || pseudo
          res ||= mod
        end
        ss
        res
      end

      def class_expr
        return unless raw '.'
        tok! :ident
      end

      def element_name
        tok(:ident) || raw('*')
      end

      def attrib
        return unless raw('[')
        ss
        tok! :ident; ss
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

      def pseudo
        return unless raw ':'
        return true if tok(:ident)
        tok! :function; ss
        tok! :ident; ss
        raw! ')'
      end

      def declaration
        return true unless property
        raw! ':'; ss
        expr! :expr
        prio
        true
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

      def expr!(name)
        return true if send(name)
        raise "Expected #{name} expression, was #{@scanner.rest.inspect}"
      end

      def tok!(name)
        return true if tok(name)
        raise "Expected #{name} token at #{@scanner.rest.inspect}"
      end

      def raw!(chr)
        return true if raw(chr)
        raise "Expected #{chr.inspect} at #{@scanner.rest.inspect}"
      end

      def tok(name)
        @scanner.scan(RX.const_get(name.to_s.upcase))
      end

      def raw(chr)
        @scanner.scan(RX.quote(chr))
      end
    end
  end
end
