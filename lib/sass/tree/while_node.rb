require 'sass/tree/node'

module Sass::Tree
  class WhileNode < Node
    def initialize(expr, options)
      @expr = expr
      super(options)
    end

    private

    def _perform(environment)
      children = []
      new_environment = Sass::Environment.new(environment)
      while @expr.perform(environment).to_bool
        children += perform_children(new_environment)
      end
      children
    end
  end
end
