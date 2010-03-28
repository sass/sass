require 'sass/script/css_parser'

module Sass
  module SCSS
    class CssParser < Parser
      private

      def variable; nil; end
      def parent_selector; nil; end
      def interpolation; nil; end
      def interp_string; tok(STRING); end
      def expected_property_separator; '":"'; end
      def use_css_import?; true; end

      def special_directive(name)
        return unless name == 'media' || name == 'import'
        super
      end

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
