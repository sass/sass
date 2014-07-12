require 'sass/script/css_parser'

module Sass
  module SCSS
    # This is a subclass of {Parser} which only parses plain CSS.
    # It doesn't support any Sass extensions, such as interpolation,
    # parent references, nested selectors, and so forth.
    # It does support all the same CSS hacks as the SCSS parser, though.
    class CssParser < StaticParser
      private

      def placeholder_selector; nil; end
      def parent_selector; nil; end
      def interpolation(warn_for_color = false); nil; end
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

      def nested_properties!(node)
        expected('expression (e.g. 1px, bold)')
      end

      def ruleset
        start_pos = source_position
        rules, source_range = selector_sequence
        return unless rules
        block(node(Sass::Tree::RuleNode.new([rules], source_range), start_pos), :ruleset)
      end

      def selector_sequence
        start_pos = source_position
        if (sel = tok(STATIC_SELECTOR, true))
          return sel, range(start_pos)
        end

        sel = selector_string
        return unless sel

        ws = ''
        while tok(/,/)
          ws << str {ss}
          if (v = selector_string)
            sel << ',' << ws << v
            ws = ''
          end
        end
        return sel, range(start_pos)
      end

      @sass_script_parser = Class.new(Sass::Script::CssParser)
      @sass_script_parser.send(:include, ScriptParser)
    end
  end
end
