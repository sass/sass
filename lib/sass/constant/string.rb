require 'sass/constant/literal'

module Sass::Constant
  class String < Literal
    
    def parse(value)
      @value = value
    end
    
    def to_s
      @value
    end
  end
end
