module Sass::Script::Functions
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
