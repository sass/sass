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

        extends[sseq] ||= []
        parent.resolved_rules.members.each {|seq| extends[sseq] << seq}
      end

      []
    end

    protected

    def perform!(environment)
      @resolved_selector = Sass::SCSS::CssParser.new(run_interp(@selector, environment)).parse_selector
      super
    end
  end
end
