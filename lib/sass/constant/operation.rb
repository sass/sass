require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'

module Sass::Constant
  class Operation
    # The regular expression matching numbers.
    NUMBER  = /^[0-9]*\.?[0-9]+$/
  
    # The regular expression matching colors.
    COLOR = /^\#(#{"[0-9a-f]" * 3}|#{"[0-9a-f]" * 6})/
  
    def initialize(operand1, operand2=nil, operator=nil)
      @operand1 = parse(operand1)
      @operand2 = parse(operand2) if operand2
      @operator = operator if operator
    end
    
    def parse(value)
      case value
        when NUMBER
          Sass::Constant::Number.new(value)
        when COLOR
          Sass::Constant::Color.new(value)
        else
          Sass::Constant::String.new(value)
      end
    end
    
    def perform
      #if @operator
      #else
        @operand1.to_s
      #end
    end
  end
end
