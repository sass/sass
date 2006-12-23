module Sass::Constant; module Literal; end; end; # Let the subclasses see the superclass

require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'

module Sass::Constant::Literal
  # The regular expression matching numbers.
  NUMBER  = /^[0-9]*\.?[0-9]+$/

  # The regular expression matching colors.
  COLOR = /^\#(#{"[0-9a-f]" * 3}|#{"[0-9a-f]" * 6})/
  
  def self.parse(value)
    case value
      when NUMBER
        Sass::Constant::Number.new(value)
      when COLOR
        Sass::Constant::Color.new(value)
      else
        Sass::Constant::String.new(value)
    end
  end
  
  def perform
    self
  end
end
