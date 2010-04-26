require 'sass/tree/node'

module Sass::Tree
  # A static node reprenting an `@extend` directive.
  #
  # @see Sass::Tree
  class ExtendNode < Node
    # @param selector [Array<String, Sass::Script::Node>]
    #   The CSS selector to extend,
    #   interspersed with {Sass::Script::Node}s
    #   representing `#{}`-interpolation.
    def initialize(selector)
      @selector = selector
      super()
    end

    # Registers this extension in the `extends` subset map.
    #
    # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
    #   The extensions defined for this tree
    # @param parent [RuleNode] The parent node of this node
    # @see Node#cssize
    def cssize(extends, parent)
      @resolved_selector.members.each do |seq|
        if seq.members.size > 1
          raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: can't extend nested selectors")
        end

        sseq = seq.members.first
        if !sseq.is_a?(Sass::Selector::SimpleSequence)
          raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: invalid selector")
        end

        sel = sseq.members
        parent.resolved_rules.members.each do |seq|
          if !seq.members.last.is_a?(Sass::Selector::SimpleSequence)
            raise Sass::SyntaxError.new("#{seq} can't extend: invalid selector")
          end

          extends[sel] = seq
        end
      end

      []
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@extend #{selector_to_src(@selector, tabs, opts, fmt).lstrip}#{semi fmt}\n"
    end

    # Runs SassScript interpolation in the selector,
    # and then parses the result into a {Sass::Selector::CommaSequence}.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    def perform!(environment)
      @resolved_selector = Sass::SCSS::CssParser.new(run_interp(@selector, environment), self.line).
        parse_selector(self.filename)
      super
    end
  end
end
