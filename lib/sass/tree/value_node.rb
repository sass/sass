require 'sass/tree/node'

module Sass::Tree
  class ValueNode < Node
    attr_accessor :value
  
    def initialize(value, style)
      @value = value
      super(style)
    end
  end
end
