# Let the subclasses see the superclass
module Sass::Constant; class Literal; end; end; # :nodoc:

require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'

class Sass::Constant::Literal # :nodoc:
  # The regular expression matching numbers.
  NUMBER  = /^(-?[0-9]*?\.?)([0-9]+)([^0-9\s]*)$/

  # The regular expression matching colors.
  COLOR = /^\#(#{"[0-9a-fA-F]" * 3}|#{"[0-9a-fA-F]" * 6})/
  
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
  
  def initialize(value = nil)
    self.parse(value) if value
  end
  
  def perform
    self
  end

  def concat(other)
    Sass::Constant::String.from_value("#{self.to_s} #{other.to_s}")
  end
  
  attr_reader :value
  
  protected
  
  def self.filter_value(value)
    value
  end
  
  def self.from_value(value)
    instance = self.new
    instance.instance_variable_set('@value', self.filter_value(value))
    instance
  end
end
