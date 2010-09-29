module Sass::Script::Functions
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

  # Converts a decimal number to a percentage.
  #
  # @example
  #   percentage(100px / 50px) => 200%
  # @param value [Number] The decimal number to convert to a percentage
  # @return [Number] The percentage
  # @raise [ArgumentError] If `value` isn't a unitless number
  def percentage(value)
    unless value.is_a?(Sass::Script::Number) && value.unitless?
      raise ArgumentError.new("#{value.inspect} is not a unitless number")
    end
    Sass::Script::Number.new(value.value * 100, ['%'])
  end

  # Rounds a number to the nearest whole number.
  #
  # @example
  #   round(10.4px) => 10px
  #   round(10.6px) => 11px
  # @param value [Number] The number
  # @return [Number] The rounded number
  # @raise [Sass::SyntaxError] if `value` isn't a number
  def round(value)
    numeric_transformation(value) {|n| n.round}
  end

  # Rounds a number up to the nearest whole number.
  #
  # @example
  #   ciel(10.4px) => 11px
  #   ciel(10.6px) => 11px
  # @param value [Number] The number
  # @return [Number] The rounded number
  # @raise [Sass::SyntaxError] if `value` isn't a number
  def ceil(value)
    numeric_transformation(value) {|n| n.ceil}
  end

  # Rounds down to the nearest whole number.
  #
  # @example
  #   floor(10.4px) => 10px
  #   floor(10.6px) => 10px
  # @param value [Number] The number
  # @return [Number] The rounded number
  # @raise [Sass::SyntaxError] if `value` isn't a number
  def floor(value)
    numeric_transformation(value) {|n| n.floor}
  end

  # Finds the absolute value of a number.
  #
  # @example
  #   abs(10px) => 10px
  #   abs(-10px) => 10px
  # @param value [Number] The number
  # @return [Number] The absolute value
  # @raise [Sass::SyntaxError] if `value` isn't a number
  def abs(value)
    numeric_transformation(value) {|n| n.abs}
  end

  private

  # This method implements the pattern of transforming a numeric value into
  # another numeric value with the same units.
  # It yields a number to a block to perform the operation and return a number
  def numeric_transformation(value)
    assert_type value, :Number
    Sass::Script::Number.new(yield(value.value), value.numerator_units, value.denominator_units)
  end

end
