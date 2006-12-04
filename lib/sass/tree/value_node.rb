require File.dirname(__FILE__) + '/../../sass'
require 'sass/tree/node'

module Sass::Tree
  class ValueNode < Node
    attr_accessor :value
  
    def initialize(value)
      @value = value
      super()
    end
    
    def to_s(tabs = 0)
      res = "#{'  ' * tabs}#{value.to_s}\n"
      children.each do |child|
        res += child.to_s(tabs + 1)
      end
      res
    end
  end
end
