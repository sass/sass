require 'sass/constant/literal'

module Sass::Constant
  class String < Literal # :nodoc:
    def plus(other)
      Sass::Constant::String.new(self.to_s + other.to_s)
    end

    def minus(other)
      Sass::Constant::String.new("#{self.to_s}-#{other.to_s}")
    end

    def unary_minus
      Sass::Constant::String.new("-#{self.to_s}")
    end

    def div(other)
      Sass::Constant::String.new("#{self.to_s}/#{other.to_s}")
    end

    def unary_div
      Sass::Constant::String.new("/#{self.to_s}")
    end

    def funcall(other)
      Sass::Constant::String.new("#{self.to_s}(#{other.to_s})")
    end

    def to_s
      @value
    end
  end
end
