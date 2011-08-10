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

      private

      def variable; nil; end
      def script_value; nil; end
      def interpolation; nil; end
      def interp_string; s = tok(STRING) and [s]; end
      def interp_ident(ident = IDENT); s = tok(ident) and [s]; end
      def use_css_import?; true; end

      def special_directive(name)
        return unless  %w[media import charset].include?(name)
        super
      end
    end
  end
end
