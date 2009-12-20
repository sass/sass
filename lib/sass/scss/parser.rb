require 'strscan'
require 'rx'

module Sass
  module SCSS
    class Parser
      def initialize(str)
        @scanner = StringScanner.new(str)
      end

      def parse
        raise @error unless catch {stylesheet}
      end

      private

      def stylesheet
        try do
          tok :charset; ss
          tok :string; ss
          raw ';'
        end

        s

        any {import; s}
        any {namespace; s}
        any do
          catch {ruleset} ||
            catch {media} ||
            catch {page} ||
            font_face
          s
        end
      end

      def s
        any do
          catch {tok :s} ||
            catch {tok :cdc} ||
            tok(:cdo)
        end
      end

      def ss
        any {tok :s}
      end

      def import
        tok :import; ss
        catch {tok :string} || tok(:uri); ss
        try do
          medium
          any {raw ','; ss; medium}
        end
        raw ';'; ss
      end

      def namespace
        tok :namespace; ss
        try {namespace_prefix; ss}
        catch {tok :string} || tok(:uri); ss
        raw ';'; ss
      end

      def namespace_prefix
        tok :ident
      end

      def media
        tok :media; ss
        medium
        any {raw ','; ss; medium}
        raw '{'; ss
        any {ruleset}
        raw '}'; ss
      end

      def medium
        tok :ident; ss
      end

      def page
        tok :page; ss
        try {tok :ident}
        try {pseudo_page}; ss
        raw '{'; ss
        declaration
        any {raw ';'; ss; declaration}
        raw '}'; ss
      end

      def pseudo_page
        raw ':'
        tok :ident
      end

      def font_face
        tok :font_face; ss
        raw '{'; ss
        declaration
        any {raw ';'; ss declaration}
        raw '}'; ss
      end

      def operator
        catch {raw '/'; ss} ||
          catch {raw ','; ss} ||
          empty
      end

      def combinator
        catch {raw '+'; ss} ||
          catch {raw '>'; ss} ||
          empty
      end

      def unary_operator
        catch {raw '-'} || raw('+')
      end

      def property
        tok :ident; ss
      end

      def ruleset
        selector
        any {raw ','; ss; selector}
        raw '{'; ss
        declaration
        any {raw ';'; ss; declaration}
        raw '}'; ss
      end

      def selector
        simple_selector
        any {combinator; simple_selector}
      end

      def simple_selector
        try {element_name}
        any do
          catch {tok :hash} ||
            catch {class_expr} ||
            catch {attrib} ||
            pseudo
        end
        ss
      end

      def class_expr
        raw '.'
        tok :ident
      end

      def element_name
        catch {tok :ident} || raw('*')
      end

      def attrib
        raw '['; ss
        tok :ident; ss
        try do
          catch {raw '='} ||
            catch {tok :includes} ||
            catch {tok :dashmatch} ||
            catch {tok :prefixmatch} ||
            catch {tok :suffixmatch} ||
            tok(:substringmatch)
          ss
          catch {tok :ident} || tok(:string); ss
        end
        raw ']'
      end

      def pseudo
        raw ':'
        catch {tok :ident} ||
          begin
            tok :function; ss
            tok :ident; ss
            raw ')'
          end
      end

      def declaration
        try do
          property
          raw ':'; ss
          expr
          try {prio}
        end
      end

      def prio
        tok :important; ss
      end

      def expr
        term
        any {operator; term}
      end

      def term
        catch do
          try {unary_operator}
          catch {tok :number; ss} || function
        end ||
          catch {tok :string; ss} ||
          catch {tok :ident; ss} ||
          catch {tok :uri; ss} ||
          catch {tok :unicoderange; ss} ||
          hexcolor
      end

      def function
        tok :function; ss
        expr
        raw ')'; ss
      end

      #  There is a constraint on the color that it must
      #  have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
      #  after the "#"; e.g., "#000" is OK, but "#abcd" is not.
      def hexcolor
        tok :hash; ss
      end

      def empty
        true
      end

      def catch(&block)
        pos = @scanner.pos
        res = Kernel.catch(:fail, &block)
        return false if res && @scanner.pos == pos
        @scanner.pos = pos unless res
        res
      end

      def try(&block)
        catch(&block) || empty
      end

      def any(&block)
        nil while catch(&block)
        true
      end

      def tok(name)
        return true if @scanner.scan(RX.const_get(name.to_s.upcase))
        @error = "Expected #{name} token at #{@scanner.rest.inspect}"
        throw :fail, false
      end

      def raw(chr)
        return true if @scanner.scan(RX.quote(chr))
        @error = "Expected #{chr.inspect} at #{@scanner.rest.inspect}"
        throw :fail, false
      end
    end
  end
end
