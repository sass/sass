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
  # @param obj [Literal] The object to inspect
  # @return [String] The unquoted string name of the literal's type
  def type_of(obj)
    Sass::Script::String.new(obj.class.name.gsub(/Sass::Script::/,'').downcase)
  end

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

  # Returns true if two numbers are similar enough to be added, subtracted, or compared.
  #
  # @example
  #   comparable(2px, 1px) => true
  #   comparable(100px, 3em) => false
  #   comparable(10cm, 3mm) => true
  # @param number1 [Number]
  # @param number2 [Number]
  # @return [Bool] indicating if the numbers can be compared.
  # @raise [ArgumentError] if `number1` or `number2` aren't numbers
  def comparable(number1, number2)
    assert_type number1, :Number
    assert_type number2, :Number
    Sass::Script::Bool.new(number1.comparable_to?(number2))
  end

end
