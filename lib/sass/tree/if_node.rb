require 'sass/tree/node'

module Sass::Tree
  class IfNode < Node
    def initialize(expr, options)
      @expr = expr
      super(options)
    end

    protected

    def _perform(environment)
      @expr.perform(environment).to_bool ? perform_children(environment) : []
    end
  end
end
