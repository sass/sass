# :stopdoc:
# Let the subclasses see the superclass
module Sass::Constant; class Literal; end; end;
# :startdoc:

require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'
require 'sass/constant/bool'

class Sass::Constant::Literal # :nodoc:
  attr_reader :value

  def initialize(value = nil)
    @value = value
  end

  def perform
    self
  end

  def and(other)
    to_bool ? other : self
  end

  def or(other)
    to_bool ? self : other
  end

  def eq(other)
    Sass::Constant::Bool.new(self.class == other.class && self.value == other.value)
  end

  def neq(other)
    Sass::Constant::Bool.new(!eq(other).to_bool)
  end

  def unary_not
    Sass::Constant::Bool.new(!to_bool)
  end

  def concat(other)
    Sass::Constant::String.new("#{self.to_s} #{other.to_s}")
  end

  def comma(other)
    Sass::Constant::String.new("#{self.to_s}, #{other.to_s}")
  end

  def inspect
    value.inspect
  end

  def to_bool
    true
  end

  def to_i
    raise SyntaxError.new("#{value.dump} is not an integer.")
  end
end
