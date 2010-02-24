module Sass
  module SCSS
    # A parser for a CSS tree.
    # This doesn't include any SCSS extensions at all.
    class CssParser < StaticParser
      private

      def parent_selector; nil; end

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

      def nested_properties!(node, expression, space)
        expected('expression (e.g. 1px, bold)');
      end
    end
  end
end
