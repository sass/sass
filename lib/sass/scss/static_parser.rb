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
      # @return [Selector::CommaSequence] The parsed selector
      # @raise [Sass::SyntaxError] if there's a syntax error in the selector
      def parse_selector
        init_scanner!
        selectors = [expr!(:_selector)]
        while tok(/,/)
          ws = str{ss}
          selectors << expr!(:_selector)
          selectors[-1] = Selector::Sequence.new(["\n"] + selectors.last.members) if ws.include?("\n")
        end
        expected("selector") unless @scanner.eos?
        Selector::CommaSequence.new(selectors)
      end

      private

      def variable; nil; end
      def script_value; nil; end
      def interpolation; nil; end
      def interp_string; s = tok(STRING) and [s]; end
      def expected_property_separator; '":"'; end
      def use_css_import?; true; end

      def special_directive(name)
        return unless name == 'media' || name == 'import'
        super
      end
    end
  end
end
