module Sass::Constant
  class UnaryOperation # :nodoc:
    def initialize(operand, operator)
      @operand = operand
      @operator = operator
    end

    def to_s
      self.perform.to_s
    end

    def inspect
      "(#{@operator.inspect} #{@operand.inspect})"
    end

    def to_arglist
      [self]
    end

    def perform
      operator = "unary_#{@operator}"
      literal = @operand.perform
      literal.send(operator)
    rescue NoMethodError => e
      raise e unless e.name.to_s == operator.to_s
      raise Sass::SyntaxError.new("Undefined unary operation: \"#{@operator} #{literal}\".")
    end
  end
end
