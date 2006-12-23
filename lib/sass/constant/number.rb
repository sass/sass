require 'sass/constant/literal'

module Sass::Constant
  class Number
    include Literal
  
    def initialize(value)
      value = value.to_f
      value = value.to_i if value % 1 == 0.0
      @value = value
    end
    
    def to_s
      @value.to_s
    end
  end
end
