require 'sass/constant/literal'

module Sass::Constant
  class Nil < Literal # :nodoc:
    def to_s
      ''
    end

    def to_arglist
      []
    end
  end
end
