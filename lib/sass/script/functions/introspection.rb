require "sass/script/functions"

module Sass::Script::Functions

  # Inspects the type of the argument, returning it as an unquoted string.
  #
  # @example
  #   type-of(100px)  => number
  #   type-of(asdf)   => string
  #   type-of("asdf") => string
  #   type-of(true)   => bool
  #   type-of(#fff)   => color
  #   type-of(blue)   => color
  # @param value [Literal] The object to inspect
  # @return [String] The unquoted string name of the literal's type
  def type_of(value)
    Sass::Script::String.new(value.class.name.gsub(/Sass::Script::/,'').downcase)
  end
  define :type_of, :args => [:value]

  # Inspects the unit of the number, returning it as a quoted string.
  # Complex units are sorted in alphabetical order by numerator and denominator.
  #
  # @example
  #   unit(100) => ""
  #   unit(100px) => "px"
  #   unit(3em) => "em"
  #   unit(10px * 5em) => "em*px"
  #   unit(10px * 5em / 30cm / 1rem) => "em*px/cm*rem"
  # @param number [Literal] The number to inspect
  # @return [String] The unit(s) of the number
  # @raise [ArgumentError] if `number` isn't a number
  def unit(number)
    assert_type number, :Number
    Sass::Script::String.new(number.unit_str, :string)
  end
  define :unit, :args => [:number]

  # Inspects the unit of the number, returning a boolean indicating if it is unitless.
  #
  # @example
  #   unitless(100) => true
  #   unitless(100px) => false
  # @param number [Literal] The number to inspect
  # @return [Bool] Whether or not the number is unitless
  # @raise [ArgumentError] if `number` isn't a number
  def unitless(number)
    assert_type number, :Number
    Sass::Script::Bool.new(number.unitless?)
  end
  define :unitless, :args => [:number]

  # Returns true if two numbers are similar enough to be added, subtracted, or compared.
  #
  # @example
  #   comparable(2px, 1px) => true
  #   comparable(100px, 3em) => false
  #   comparable(10cm, 3mm) => true
  # @param number_1 [Number]
  # @param number_2 [Number]
  # @return [Bool] indicating if the numbers can be compared.
  # @raise [ArgumentError] if `number_1` or `number_2` aren't numbers
  def comparable(number_1, number_2)
    assert_type number_1, :Number
    assert_type number_2, :Number
    Sass::Script::Bool.new(number_1.comparable_to?(number_2))
  end
  define :comparable, :args => [:number_1, :number_2]

end
