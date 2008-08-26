require 'sass/constant/literal'

module Sass::Constant
  class Bool < Literal # :nodoc:

    def parse(value)
      first, second, unit = value.scan(Literal::NUMBER)[0]
      @value = value == 'true' ? true : false
    end

    def to_s
      @value.to_s
    end

    def to_bool
      @value
    end
  end
end
