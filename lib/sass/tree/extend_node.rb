require 'sass/tree/node'

module Sass::Tree
  class ExtendNode < Node
    def initialize(selector)
      @selector = selector
      super()
    end

    protected

    def perform!(environment)
      @resolved_selector = run_interp(@selector, environment)
      super
    end

    def _cssize(extends, parent)
      extends[@resolved_selector] ||= []
      extends[@resolved_selector] += parent.resolved_rules
    end
  end
end
