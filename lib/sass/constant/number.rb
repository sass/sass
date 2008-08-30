require 'sass/constant/literal'

module Sass::Constant  # :nodoc:
  class Number < Literal # :nodoc:

    attr_reader :unit

    def parse(value)
      first, second, unit = value.scan(Literal::NUMBER)[0]
      @value = first.empty? ? second.to_i : "#{first}#{second}".to_f
      @unit = unit.empty? ? nil : unit
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
      "#{value}#{@unit}"
    end

    protected

    def self.from_value(value, unit=nil)
      instance = super(value)
      instance.instance_variable_set('@unit', unit)
      instance
    end

    def operate(other, operation)
      unit = nil
      if other.unit.nil?
        unit = self.unit
      elsif self.unit.nil?
        unit = other.unit
      elsif other.unit == self.unit
        unit = self.unit
      else
        raise Sass::SyntaxError.new("Incompatible units: #{self.unit} and #{other.unit}.")
      end

      Number.from_value(self.value.send(operation, other.value), unit)
    end
  end
end
