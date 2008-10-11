require 'sass/constant/literal'

module Sass::Constant
  class Bool < Literal # :nodoc:
    def to_s
      @value.to_s
    end

    def to_bool
      @value
    end
  end
end
