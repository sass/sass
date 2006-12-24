require 'sass/constant/literal'

module Sass::Constant
  class Number < Literal
  
    def parse(value)
      value = value.to_f
      value = value.to_i if value % 1 == 0.0
      @value = value
    end
    
    def plus(other)
      if other.is_a? Number
        Number.from_value(self.value + other.value)
      else
        Sass::Constant::String.from_value(self.to_s + other.to_s)
      end
    end
    
    def to_s
      @value.to_s
    end
  end
end
