require 'sass/constant/literal'

module Sass::Constant
  class Number < Literal
  
    def parse(value)
      value = value.to_f
      value = self.class.filter_value(value)
      @value = value
    end
    
    def plus(other)
      if other.is_a? Number
        operate(other, :+)
      elsif other.is_a? Color
        other.plus(self)
      else
        Sass::Constant::String.from_value(self.to_s + other.to_s)
      end
    end
    
    def times(other)
      if other.is_a? Number
        operate(other, :*)
      elsif other.is_a? Color
        other.times(self)
      else
        raise NoMethodError.new(nil, :times)
      end
    end
    
    def to_s
      @value.to_s
    end
    
    protected
    
    def operate(other, operation)
      Number.from_value(self.value.send(operation, other.value))
    end
    
    def self.filter_value(value)
      if value % 1 == 0.0
        value.to_i
      else
        value
      end
    end
  end
end
