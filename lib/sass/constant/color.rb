require 'sass/constant/literal'

module Sass::Constant
  class Color < Literal
  
    REGEXP = /\##{"([0-9a-f]{1,2})" * 3}/
  
    def parse(value)
      @value = value.scan(REGEXP)[0].map { |num| num.ljust(2, 'f').to_i(16) }
    end
    
    def to_s
      red, green, blue = @value.map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end
  end
end
