# :stopdoc:
# Let the subclasses see the superclass
module Sass::Constant; class Literal; end; end;
# :startdoc:

require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'
require 'sass/constant/nil'

class Sass::Constant::Literal # :nodoc:
  # The regular expression matching numbers.
  NUMBER  = /^(-?\d*?\.?)(\d+)([^\d\s]*)$/

  html_color_matcher = Sass::Constant::Color::HTML4_COLORS.keys.map { |c| "^#{c}$" }.join '|'

  # The regular expression matching colors.
  COLOR = /^\# (?: [\da-f]{3} | [\da-f]{6} ) | #{html_color_matcher}/ix

  def self.parse(value)
    case value
    when NUMBER
      Sass::Constant::Number.new(value)
    when COLOR
      Sass::Constant::Color.new(value)
    when ::Symbol
      value
    else
      Sass::Constant::String.new(value)
    end
  end

  def initialize(value = nil)
    if value.is_a?(String)
      self.parse(value)
    else
      @value = value
    end
  end

  def perform
    self
  end

  def concat(other)
    Sass::Constant::String.from_value("#{self.to_s} #{other.to_s}")
  end

  def comma(other)
    Sass::Constant::String.from_value("#{self.to_s}, #{other.to_s}")
  end

  def inspect
    value.inspect
  end

  def to_arglist
    [self]
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
