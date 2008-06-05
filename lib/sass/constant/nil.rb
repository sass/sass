require 'sass/constant/literal'

module Sass::Constant # :nodoc:
  class Nil < Literal # :nodoc:
    def to_s
      ''
    end
  end
end
