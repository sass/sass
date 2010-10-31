require 'sass/script/css_parser'

module Sass
  module SCSS
    # This is a subclass of {Parser} which only parses plain CSS.
    # It doesn't support any Sass extensions, such as interpolation,
    # parent references, nested selectors, and so forth.
    # It does support all the same CSS hacks as the SCSS parser, though.
    class CssParser < StaticParser
      # Parse a selector, and return its value as a string.
      #
      # @return [String, nil] The parsed selector, or nil if no selector was parsed
      # @raise [Sass::SyntaxError] if there's a syntax error in the selector
      def parse_selector_string
        init_scanner!
        str {return unless selector}
      end

      private

      def parent_selector; nil; end
      def interpolation; nil; end
      def interp_string; tok(STRING); end
      def interp_ident(ident = IDENT); tok(ident); end
      def use_css_import?; true; end

      def block_child(context)
        case context
        when :ruleset
          declaration
        when :stylesheet
          directive || ruleset
        when :directive
          directive || declaration_or_ruleset
        end
      end

      def nested_properties!(node, space)
        expected('expression (e.g. 1px, bold)');
      end

      @sass_script_parser = Class.new(Sass::Script::CssParser)
      @sass_script_parser.send(:include, ScriptParser)
    end
  end
end
