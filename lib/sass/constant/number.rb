require 'sass/constant/literal'

module Sass::Constant
  class Number < Literal
  
    def parse(value)
      value = value.to_f
      value = value.to_i if value % 1 == 0.0
      @value = value
    end
    
    def plus(other)
      Number.from_value(self.value + other.value)
    end
    
    def to_s
      @value.to_s
    end
  end
end
