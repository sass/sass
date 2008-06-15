require 'sass/constant/literal'

module Sass::Constant
  class String < Literal # :nodoc:

    def parse(value)
      @value = value
    end

    def plus(other)
      Sass::Constant::String.from_value(self.to_s + other.to_s)
    end

    def minus(other)
      Sass::Constant::String.from_value("#{self.to_s}-#{other.to_s}")
    end

    def unary_minus
      Sass::Constant::String.from_value("-#{self.to_s}")
    end

    def div(other)
      Sass::Constant::String.from_value("#{self.to_s}/#{other.to_s}")
    end

    def unary_div
      Sass::Constant::String.from_value("/#{self.to_s}")
    end

    def funcall(other)
      Sass::Constant::String.from_value("#{self.to_s}(#{other.to_s})")
    end

    def to_s
      @value
    end
  end
end
