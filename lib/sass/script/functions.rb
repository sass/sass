module Sass::Script
  # Methods in this module are accessible from the SassScript context.
  # For example, you can write
  #
  #     !color = hsl(120, 100%, 50%)
  #
  # and it will call {Sass::Script::Functions#hsl}.
  #
  # The following functions are provided:
  #
  # \{#hsl}
  # : Converts an `hsl(hue, saturation, lightness)` triplet into a color.
  #
  # \{#percentage}
  # : Converts a unitless number to a percentage.
  #
  # \{#round}
  # : Rounds a number to the nearest whole number.
  #
  # \{#ceil}
  # : Rounds a number up to the nearest whole number.
  #
  # \{#floor}
  # : Rounds a number down to the nearest whole number.
  #
  # \{#abs}
  # : Returns the absolute value of a number.
  #
  # You can add your own functions to this module,
  # but there are a few things to keep in mind.
  # First of all, the arguments passed are {Sass::Script::Literal} objects.
  # Literal objects are also expected to be returned.
  #
  # Second, making Ruby functions accessible from Sass introduces the temptation
  # to do things like database access within stylesheets.
  # This temptation must be resisted.
  # Keep in mind that Sass stylesheets are only compiled once
  # at a somewhat indeterminate time
  # and then left as static CSS files.
  # Any dynamic CSS should be left in `<style>` tags in the HTML.
  #
  # Within one of the functions in this module,
  # methods of {EvaluationContext} can be used.
  module Functions
    # The context in which methods in {Script::Functions} are evaluated.
    # That means that all instance methods of {EvaluationContext}
    # are available to use in functions.
    class EvaluationContext
      include Sass::Script::Functions

      # The options hash for the {Sass::Engine} that is processing the function call
      #
      # @return [Hash<Symbol, Object>]
      attr_reader :options

      # @param options [Hash<Symbol, Object>] See \{#options}
      def initialize(options)
        @options = options
      end
    end

    instance_methods.each { |m| undef_method m unless m.to_s =~ /^__/ }


    # Creates a {Color} object from red, green, and blue values.
    # @param red
    #   A number between 0 and 255 inclusive
    # @param green
    #   A number between 0 and 255 inclusive
    # @param blue
    #   A number between 0 and 255 inclusive
    def rgb(red, green, blue)
      [red.value, green.value, blue.value].each do |v|
        raise ArgumentError.new("Color value #{v} must be between 0 and 255 inclusive") if v <= 0 || v >= 255
      end
      Color.new([red.value, green.value, blue.value])
    end

    # Creates a {Color} object from hue, saturation, and lightness
    # as per the CSS3 spec (http://www.w3.org/TR/css3-color/#hsl-color).
    #
    # @param hue [Number] The hue of the color.
    #   Should be between 0 and 360 degrees, inclusive
    # @param saturation [Number] The saturation of the color.
    #   Must be between `0%` and `100%`, inclusive
    # @param lightness [Number] The lightness of the color.
    #   Must be between `0%` and `100%`, inclusive
    # @return [Color] The resulting color
    # @raise [ArgumentError] if `saturation` or `lightness` are out of bounds
    def hsl(hue, saturation, lightness)
      original_s = saturation
      original_l = lightness
      # This algorithm is from http://www.w3.org/TR/css3-color#hsl-color
      h, s, l = [hue, saturation, lightness].map { |a| a.value }
      raise ArgumentError.new("Saturation #{s} must be between 0% and 100%") if s < 0 || s > 100
      raise ArgumentError.new("Lightness #{l} must be between 0% and 100%") if l < 0 || l > 100

      h = (h % 360) / 360.0
      s /= 100.0
      l /= 100.0

      m2 = l <= 0.5 ? l * (s + 1) : l + s - l * s
      m1 = l * 2 - m2
      Color.new([hue_to_rgb(m1, m2, h + 1.0/3),
                 hue_to_rgb(m1, m2, h),
                 hue_to_rgb(m1, m2, h - 1.0/3)].map { |c| (c * 0xff).round })
    end

    # Converts a decimal number to a percentage.
    # For example:
    #
    #     percentage(100px / 50px) => 200%
    #
    # @param value [Number] The decimal number to convert to a percentage
    # @return [Number] The percentage
    # @raise [ArgumentError] If `value` isn't a unitless number
    def percentage(value)
      unless value.is_a?(Sass::Script::Number) && value.unitless?
        raise ArgumentError.new("#{value} is not a unitless number")
      end
      Sass::Script::Number.new(value.value * 100, ['%'])
    end

    # Rounds a number to the nearest whole number.
    # For example:
    #
    #     round(10.4px) => 10px
    #     round(10.6px) => 11px
    #
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [Sass::SyntaxError] if `value` isn't a number
    def round(value)
      numeric_transformation(value) {|n| n.round}
    end

    # Rounds a number up to the nearest whole number.
    # For example:
    #
    #     ciel(10.4px) => 11px
    #     ciel(10.6px) => 11px
    #
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [Sass::SyntaxError] if `value` isn't a number
    def ceil(value)
      numeric_transformation(value) {|n| n.ceil}
    end

    # Rounds down to the nearest whole number.
    # For example:
    #
    #     floor(10.4px) => 10px
    #     floor(10.6px) => 10px
    #
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [Sass::SyntaxError] if `value` isn't a number
    def floor(value)
      numeric_transformation(value) {|n| n.floor}
    end

    # Finds the absolute value of a number.
    # For example:
    #
    #     abs(10px) => 10px
    #     abs(-10px) => 10px
    #
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
      unless value.is_a?(Sass::Script::Number)
        calling_function = caller.first.scan(/`([^']+)'/).first.first
        raise Sass::SyntaxError.new("#{value} is not a number for `#{calling_function}'")
      end
      Sass::Script::Number.new(yield(value.value), value.numerator_units, value.denominator_units)
    end

    def hue_to_rgb(m1, m2, h)
      h += 1 if h < 0
      h -= 1 if h > 1
      return m1 + (m2 - m1) * h * 6 if h * 6 < 1
      return m2 if h * 2 < 1
      return m1 + (m2 - m1) * (2.0/3 - h) * 6 if h * 3 < 2
      return m1
    end
  end
end
