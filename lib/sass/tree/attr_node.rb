require File.dirname(__FILE__) + '/../../sass'
require 'sass/tree/node'

module Sass::Tree
  class AttrNode < ValueNode
    attr_accessor :name
    
    def initialize(name, value)
      @name = name
      value = value.join('  ') if value.is_a? Array
      super(value)
    end
    
    def to_s
      "#{name}: #{value};"
    end
  end
end
