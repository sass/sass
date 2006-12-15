require File.dirname(__FILE__) + '/../../sass'
require 'sass/tree/node'

module Sass::Tree
  class ValueNode < Node
    attr_accessor :value
  
    def initialize(value)
      @value = value
      super()
    end
  end
end
