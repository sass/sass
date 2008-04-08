require 'sass/constant/literal'

module Sass::Constant # :nodoc:
  class String < Literal # :nodoc:

    def parse(value)
      @value = value
    end

    def plus(other)
      Sass::Constant::String.from_value(self.to_s + other.to_s)
    end

    def funcall(other)
      Sass::Constant::String.from_value("#{self.to_s}(#{other.to_s})")
    end

    def to_s
      @value
    end
  end
end
