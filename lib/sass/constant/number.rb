require 'sass/constant/literal'

module Sass::Constant
  class Number < Literal
  
    def parse(value)
      value = value.include?('.') ? value.to_f : value.to_i
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
    
    def minus(other)
      if other.is_a? Number
        operate(other, :-)
      else
        raise NoMethodError.new(nil, :minus)
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
    
    def div(other)
      if other.is_a? Number
        operate(other, :/)
      else
        raise NoMethodError.new(nil, :div)
      end
    end
    
    def mod(other)
      if other.is_a? Number
        operate(other, :%)
      else
        raise NoMethodError.new(nil, :mod)
      end
    end
    
    def to_s
      value = @value
      value = value.to_i if value % 1 == 0.0
      value.to_s
    end
    
    protected
    
    def operate(other, operation)
      Number.from_value(self.value.send(operation, other.value))
    end
  end
end
