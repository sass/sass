module Sass
  module SCSS
    class CssParser < Parser
      private

      def scss_directive(name); nil; end
      def variable; nil; end
      def parent_selector; nil; end
      def script_value; nil; end
      def interpolation; nil; end
      def interp_string; tok(STRING); end
      def expected_property_separator; '":"'; end

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
