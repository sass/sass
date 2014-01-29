require 'sass/script/css_parser'

module Sass
  module SCSS
    # A parser for a static SCSS tree.
    # Parses with SCSS extensions, like nested rules and parent selectors,
    # but without dynamic SassScript.
    # This is useful for e.g. \{#parse\_selector parsing selectors}
    # after resolving the interpolation.
    class StaticParser < Parser
      # Parses the text as a selector.
      #
      # @param filename [String, nil] The file in which the selector appears,
      #   or nil if there is no such file.
      #   Used for error reporting.
      # @return [Selector::CommaSequence] The parsed selector
      # @raise [Sass::SyntaxError] if there's a syntax error in the selector
      def parse_selector
        init_scanner!
        seq = expr!(:selector_comma_sequence)
        expected("selector") unless @scanner.eos?
        seq.line = @line
        seq.filename = @filename
        seq
      end

      # Parses a static at-root query.
      #
      # @return [(Symbol, Array<String>)] The type of the query
      #   (`:with` or `:without`) and the values that are being filtered.
      # @raise [Sass::SyntaxError] if there's a syntax error in the query,
      #   or if it doesn't take up the entire input string.
      def parse_static_at_root_query
        init_scanner!
        tok!(/\(/); ss
        type = tok!(/\b(without|with)\b/).to_sym; ss
        tok!(/:/); ss
        directives = expr!(:at_root_directive_list); ss
        tok!(/\)/)
        expected("@at-root query list") unless @scanner.eos?
        return type, directives
      end

      private

      def moz_document_function
        val = tok(URI) || tok(URL_PREFIX) || tok(DOMAIN) || function(!:allow_var)
        return unless val
        ss
        [val]
      end

      def variable; nil; end
      def script_value; nil; end
      def interpolation; nil; end
      def var_expr; nil; end
      def interp_string; (s = tok(STRING)) && [s]; end
      def interp_uri; (s = tok(URI)) && [s]; end
      def interp_ident(ident = IDENT); (s = tok(ident)) && [s]; end
      def use_css_import?; true; end

      def special_directive(name, start_pos)
        return unless %w[media import charset -moz-document].include?(name)
        super
      end

      @sass_script_parser = Class.new(Sass::Script::CssParser)
      @sass_script_parser.send(:include, ScriptParser)
    end
  end
end
