module Sass::Constant
  class Color
    REGEXP = /\##{"([0-9a-f]{1,2})" * 3}/
  
    def initialize(value)
      @red, @green, @blue = value.scan(REGEXP)[0].map { |num| num.ljust(2, 'f').to_i(16) }
    end
    
    def to_s
      red, green, blue = [@red, @green, @blue].map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end
  end
end
