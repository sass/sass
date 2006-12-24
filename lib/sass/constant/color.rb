require 'sass/constant/literal'

module Sass::Constant
  class Color < Literal
  
    REGEXP = /\##{"([0-9a-f]{1,2})" * 3}/
  
    def parse(value)
      @value = value.scan(REGEXP)[0].map { |num| num.ljust(2, 'f').to_i(16) }
    end
    
    def plus(other)
      if other.is_a? Sass::Constant::String
        Sass::Constant::String.from_value(self.to_s + other.to_s)
      else
        Color.piecewise(self, other) { |val1, val2| [val1 + val2, 255].min }
      end
    end
    
    def to_s
      red, green, blue = @value.map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end
    
    protected
    
    def self.piecewise(color1, other)
      other_num = other.is_a? Number
      other_val = other.value
    
      rgb = []
      for i in (0...3)
        rgb[i] = yield(color1.value[i], other_num ? other_val : other_val[i])
      end
      Color.from_value(rgb)
    end
  end
end
