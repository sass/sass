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
      def interpolation; nil; end
      def use_css_import?; true; end

      def block_child(context)
        old_block_context, @block_context = @block_context, context
        case context
        when :stylesheet;      directive || ruleset
        when :keyframes;       directive || keyframes_block
        when :keyframes_block; directive || declaration
        when :ruleset;         declaration
        when :directive;       declaration_or_ruleset
        else raise "[BUG] Unknown block_child context #{context.inspect}"
        end
      ensure
        @block_context = old_block_context
      end

      def nested_properties!(node, space)
        expected('expression (e.g. 1px, bold)')
      end

      @sass_script_parser = Class.new(Sass::Script::CssParser)
      @sass_script_parser.send(:include, ScriptParser)
    end
  end
end
