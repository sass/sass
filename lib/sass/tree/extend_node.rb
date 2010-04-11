require 'sass/tree/node'

module Sass::Tree
  class ExtendNode < Node
    def initialize(selector)
      @selector = selector
      super()
    end

    def cssize(extends, parent)
      @resolved_selector.members.each do |seq|
        if seq.members.size > 1
          raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: can't extend nested selectors")
        end

        sseq = seq.members.first
        if !sseq.is_a?(Sass::Selector::SimpleSequence)
          raise Sass::SyntaxError.new("Can't extend #{seq.to_a.join}: invalid selector")
        end

        sel = sseq.members.to_set
        parent.resolved_rules.members.each do |seq|
          if seq.members.size > 1
            raise Sass::SyntaxError.new("#{seq.to_a.join} can't extend: nested selectors can't extend")
          end

          sseq = seq.members.first
          if !sseq.is_a?(Sass::Selector::SimpleSequence)
            raise Sass::SyntaxError.new("#{seq.to_a.join} can't extend: invalid selector")
          end

          extends[sel] = sseq
        end
      end

      []
    end

    protected

    def perform!(environment)
      @resolved_selector = Sass::SCSS::CssParser.new(run_interp(@selector, environment)).
        parse_selector(self.line, self.filename)
      super
    end
  end
end
