require 'sass/tree/node'

module Sass::Tree
  class ValueNode < Node
    attr_accessor :value

    def initialize(value, style)
      @value = value
      super(style)
    end

    def to_s(tabs = 0)
      value
    end
  end
end
