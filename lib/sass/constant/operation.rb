require 'sass/constant/string'

module Sass::Constant
  class Operation
    def initialize(operand1, operand2=nil, operator=nil)
      @operand1 = parse(operand1)
      @operand2 = parse(operand2) if operand2
      @operator = operator if operator
    end
    
    def parse(value)
      Sass::Constant::String.new(value)
    end
    
    def perform(constants)
      if @operator
      else
        @operand1.to_s(constants)
      end
    end
  end
end
