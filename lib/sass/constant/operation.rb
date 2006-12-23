require 'sass/constant/string'
require 'sass/constant/number'

module Sass::Constant
  class Operation
    def initialize(operand1, operand2=nil, operator=nil)
      @operand1 = parse(operand1)
      @operand2 = parse(operand2) if operand2
      @operator = operator if operator
    end
    
    def parse(value)
      case value
        when /^[0-9]*\.?[0-9]+$/ # Number with one or zero decimal points
          Sass::Constant::Number.new(value)
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
