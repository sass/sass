require 'sass/tree/node'

module Sass::Tree
  class IfNode < Node
    attr_accessor :else

    def initialize(expr)
      @expr = expr
      @last_else = self
      super()
    end

    def add_else(node)
      @last_else.else = node
      @last_else = node
    end

    def options=(options)
      super
      self.else.options = options if self.else
    end

    protected

    def _perform(environment)
      environment = Sass::Environment.new(environment)
      return perform_children(environment) if @expr.nil? || @expr.perform(environment).to_bool
      return @else.perform(environment) if @else
      []
    end
  end
end
