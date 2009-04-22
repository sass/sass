require 'sass/tree/node'

module Sass::Tree
  class WhileNode < Node
    def initialize(expr)
      @expr = expr
      super()
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
