module Sass::Script::Value
  # Provides helper functions for creating sass values from within ruby methods.
  # @since `3.3.0`
  module Helpers
    # Construct a Sass Boolean.
    #
    # @param value [Object] A ruby object that will be tested for truthiness.
    # @return [Sass::Script::Value::Bool] whether the ruby value is truthy.
    def bool(value)
      Bool.new(value)
    end

    # Construct a Sass Color from a hex color string.
    #
    # @param value [::String] A string representing a hex color.
    #   The leading hash ("#") is optional.
    # @param alpha [::Number] The alpha channel. A number between 0 and 1.
    # @return [Sass::Script::Value::Color] the color object
    def hex_color(value, alpha = nil)
      Color.from_hex(value, alpha)
    end

    # Construct a Sass Color from hsl values.
    #
    # @param hue [::Number] The hue of the color in degrees.
    #   A non-negative number, usually less than 360.
    # @param saturation [::Number] The saturation of the color.
    #   Must be between 0 and 100 inclusive.
    # @param lightness [::Number] The lightness of the color.
    #   Must be between 0 and 100 inclusive.
    # @param alpha [::Number] The alpha channel. A number between 0 and 1.
    #
    # @return [Sass::Script::Value::Color] the color object
    def hsl_color(hue, saturation, lightness, alpha = nil)
      attrs = {:hue => hue, :saturation => saturation, :lightness => lightness}
      attrs[:alpha] = alpha if alpha
      Color.new(attrs)
    end

    # Construct a Sass Color from rgb values.
    #
    # @param red [::Number] The red component. Must be between 0 and 255 inclusive.
    # @param green [::Number] The green component. Must be between 0 and 255 inclusive.
    # @param blue [::Number] The blue component. Must be between 0 and 255 inclusive.
    # @param alpha [::Number] The alpha channel. A number between 0 and 1.
    #
    # @return [Sass::Script::Value::Color] the color object
    def rgb_color(red, green, blue, alpha = nil)
      attrs = {:red => red, :green => green, :blue => blue}
      attrs[:alpha] = alpha if alpha
      Color.new(attrs)
    end

    # Construct a Sass Number from a ruby number.
    #
    # @param number [::Number] A numeric value.
    # @param unit_string [::String] A unit string of the form
    #   `numeral_unit1 * numeral_unit2 ... / denominator_unit1 * denominator_unit2 ...`
    #   this is the same format that is returned by
    #   {Sass::Script::Value::Number#unit_str the `unit_str` method}
    #
    # @see Sass::Script::Value::Number#unit_str
    #
    # @return [Sass::Script::Value::Number] The sass number representing the given ruby number.
    def number(number, unit_string = nil)
      Number.new(number, *parse_unit_string(unit_string))
    end

    # @overload list(*elements, separator)
    #   Create a space-separated list from the arguments given.
    #   @param elements [Array<Sass::Script::Value::Base>] Each argument will be a list element.
    #   @param separator [Symbol] Either :space or :comma.
    #   @return [Sass::Script::Value::List] The space separated list.
    #
    # @overload list(array, separator)
    #   Create a space-separated list from the array given.
    #   @param array [Array<Sass::Script::Value::Base>] A ruby array of Sass values
    #     to make into a list.
    #   @return [Sass::Script::Value::List] The space separated list.
    def list(*elements)
      unless elements.last.is_a?(Symbol)
        raise ArgumentError.new("A list type of :space or :comma must be specified.")
      end
      separator = elements.pop
      if elements.size == 1 && elements.first.is_a?(Array)
        elements = elements.first
      end
      Sass::Script::Value::List.new(elements, separator)
    end

    # Construct a Sass map.
    #
    # @param hash [Hash<Sass::Script::Value::Base,
    #   Sass::Script::Value::Base>] A Ruby map to convert to a Sass map.
    # @return [Sass::Script::Value::Map] The map.
    def map(hash)
      Map.new(hash)
    end

    # Create a sass null value.
    #
    # @return [Sass::Script::Value::Null]
    def null
      Sass::Script::Value::Null.new
    end

    # Create a quoted string.
    #
    # @param str [::String] A ruby string.
    # @return [Sass::Script::Value::String] A quoted string.
    def quoted_string(str)
      Sass::Script::String.new(str, :string)
    end

    # Create an unquoted string.
    #
    # @param str [::String] A ruby string.
    # @return [Sass::Script::Value::String] An unquoted string.
    def unquoted_string(str)
      Sass::Script::String.new(str, :identifier)
    end
    alias_method :identifier, :unquoted_string

    private

    # @private
    VALID_UNIT = /#{Sass::SCSS::RX::NMSTART}#{Sass::SCSS::RX::NMCHAR}|%*/

    # @example
    #   parse_unit_string("em*px/in*%") # => [["em", "px], ["in", "%"]]
    #
    # @param unit_string [String] A string adhering to the output of a number with complex
    #   units. E.g. "em*px/in*%"
    # @return [Array<Array<String>>] A list of numerator units and a list of denominator units.
    def parse_unit_string(unit_string)
      denominator_units = numerator_units = Sass::Script::Value::Number::NO_UNITS
      return numerator_units, denominator_units unless unit_string && unit_string.length > 0
      num_over_denominator = unit_string.split(/ *\/ */)
      unless (1..2).include?(num_over_denominator.size)
        raise ArgumentError.new("Malformed unit string: #{unit_string}")
      end
      numerator_units = num_over_denominator[0].split(/ *\* */)
      denominator_units = (num_over_denominator[1] || "").split(/ *\* */)
      [[numerator_units, "numerator"], [denominator_units, "denominator"]].each do |units, name|
        if unit_string =~ /\// && units.size == 0
          raise ArgumentError.new("Malformed unit string: #{unit_string}")
        end
        if units.any? {|unit| unit !~ VALID_UNIT}
          raise ArgumentError.new("Malformed #{name} in unit string: #{unit_string}")
        end
      end
      [numerator_units, denominator_units]
    end
  end
end
