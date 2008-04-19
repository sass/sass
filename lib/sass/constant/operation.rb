require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'

module Sass::Constant # :nodoc:
  class Operation # :nodoc:
    def initialize(operand1, operand2, operator)
      @operand1 = operand1
      @operand2 = operand2
      @operator = operator
    end

    def to_s
      self.perform.to_s
    end

    protected

    def perform
      literal1 = @operand1.perform
      literal2 = @operand2.perform
      begin
        literal1.send(@operator, literal2)
      rescue NoMethodError => e
        raise e unless e.name.to_s == @operator.to_s
        raise Sass::SyntaxError.new("Undefined operation: \"#{literal1} #{@operator} #{literal2}\".")
      end
    end
  end
end
