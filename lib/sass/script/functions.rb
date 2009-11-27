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
  # \{#hsla}
  # : Converts an `hsla(hue, saturation, lightness, alpha)` quadruplet into a color.
  #
  # \{#rgb}
  # : Converts an `rgb(red, green, blue)` triplet into a color.
  #
  # \{#rgba}
  # : Converts an `rgb(red, green, blue, alpha)` triplet into a color.
  #
  # \{#red}
  # : Gets the red component of a color.
  #
  # \{#green}
  # : Gets the green component of a color.
  #
  # \{#blue}
  # : Gets the blue component of a color.
  #
  # \{#alpha} / \{#opacity}
  # : Gets the alpha component (opacity) of a color.
  #
  # \{#opacify} / \{#fade_in #fade-in}
  # : Makes a color more opaque.
  #
  # \{#transparentize} / \{#fade_out #fade-out}
  # : Makes a color more transparent.
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
  # These functions are described in more detail below.
  #
  # ## Adding Custom Functions
  #
  # New Sass functions can be added by adding Ruby methods to this module.
  # For example:
  #
  #     module Sass::Script::Functions
  #       def reverse(string)
  #         assert_type string, :String
  #         Sass::Script::String.new(string.value.reverse)
  #       end
  #     end
  #
  # There are a few things to keep in mind when modifying this module.
  # First of all, the arguments passed are {Sass::Script::Literal} objects.
  # Literal objects are also expected to be returned.
  # This means that Ruby values must be unwrapped and wrapped.
  #
  # Most Literal objects support the {Sass::Script::Literal#value value} accessor
  # for getting their Ruby values.
  # Color objects, though, must be accessed using {Sass::Script::Color#rgb rgb},
  # {Sass::Script::Color#red red}, {Sass::Script::Color#blue green}, or {Sass::Script::Color#blue blue}.
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

      # Asserts that the type of a given SassScript value
      # is the expected type (designated by a symbol).
      # For example:
      #
      #     assert_type value, :String
      #     assert_type value, :Number
      #
      # Valid types are `:Bool`, `:Color`, `:Number`, and `:String`.
      #
      # @param value [Sass::Script::Literal] A SassScript value
      # @param type [Symbol] The name of the type the value is expected to be
      def assert_type(value, type)
        return if value.is_a?(Sass::Script.const_get(type))
        raise ArgumentError.new("#{value.inspect} is not a #{type.to_s.downcase}")
      end
    end

    instance_methods.each { |m| undef_method m unless m.to_s =~ /^__/ }


    # Creates a {Color} object from red, green, and blue values.
    # @param red
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    # @param green
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    # @param blue
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    def rgb(red, green, blue)
      rgba(red, green, blue, Number.new(1))
    end

    # Creates a {Color} object from red, green, and blue values,
    # as well as an alpha channel indicating opacity.
    #
    # @param red
    #   A number between 0 and 255 inclusive
    # @param green
    #   A number between 0 and 255 inclusive
    # @param blue
    #   A number between 0 and 255 inclusive
    # @param alpha
    #   A number between 0 and 1
    def rgba(red, green, blue, alpha)
      assert_type red, :Number
      assert_type green, :Number
      assert_type blue, :Number
      assert_type alpha, :Number

      rgb = [red, green, blue].map do |c|
        v = c.value
        if c.numerator_units == ["%"] && c.denominator_units.empty?
          next v * 255 / 100.0 if (0..100).include?(v)
          raise ArgumentError.new("Color value #{c} must be between 0% and 100% inclusive")
        else
          next v if (0..255).include?(v)
          raise ArgumentError.new("Color value #{v} must be between 0 and 255 inclusive")
        end
      end

      unless (0..1).include?(alpha.value)
        raise ArgumentError.new("Alpha channel #{alpha.value} must be between 0 and 1 inclusive")
      end

      Color.new(rgb + [alpha.value])
    end

    # Creates a {Color} object from hue, saturation, and lightness.
    # Uses the algorithm from the [CSS3 spec](http://www.w3.org/TR/css3-color/#hsl-color).
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
      hsla(hue, saturation, lightness, Number.new(1))
    end

    # Creates a {Color} object from hue, saturation, and lightness,
    # as well as an alpha channel indicating opacity.
    # Uses the algorithm from the [CSS3 spec](http://www.w3.org/TR/css3-color/#hsl-color).
    #
    # @param hue [Number] The hue of the color.
    #   Should be between 0 and 360 degrees, inclusive
    # @param saturation [Number] The saturation of the color.
    #   Must be between `0%` and `100%`, inclusive
    # @param lightness [Number] The lightness of the color.
    #   Must be between `0%` and `100%`, inclusive
    # @param alpha [Number] The opacity of the color.
    #   Must be between 0 and 1, inclusive
    # @return [Color] The resulting color
    # @raise [ArgumentError] if `saturation`, `lightness`, or `alpha` are out of bounds
    def hsla(hue, saturation, lightness, alpha)
      assert_type hue, :Number
      assert_type saturation, :Number
      assert_type lightness, :Number
      assert_type alpha, :Number

      unless (0..1).include?(alpha.value)
        raise ArgumentError.new("Alpha channel #{alpha.value} must be between 0 and 1")
      end

      original_s = saturation
      original_l = lightness
      # This algorithm is from http://www.w3.org/TR/css3-color#hsl-color
      h, s, l = [hue, saturation, lightness].map { |a| a.value }
      raise ArgumentError.new("Saturation #{s} must be between 0% and 100%") unless (0..100).include?(s)
      raise ArgumentError.new("Lightness #{l} must be between 0% and 100%") unless (0..100).include?(l)

      Color.new(:hue => h, :saturation => s, :lightness => l, :alpha => alpha.value)
    end

    # Returns the red component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def red(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.red)
    end

    # Returns the green component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def green(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.green)
    end

    # Returns the blue component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def blue(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.blue)
    end

    # Returns the hue component of a color.
    #
    # See [the CSS3 HSL specification](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # Calculated from RGB where necessary via [this algorithm](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # @param color [Color]
    # @return [Number] between 0 and 360
    # @raise [ArgumentError] if `color` isn't a color
    def hue(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.hue)
    end

    # Returns the saturation component of a color.
    #
    # See [the CSS3 HSL specification](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # Calculated from RGB where necessary via [this algorithm](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # @param color [Color]
    # @return [Number] between 0% and 100%
    # @raise [ArgumentError] if `color` isn't a color
    def saturation(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.saturation, ["%"])
    end

    # Returns the hue component of a color.
    #
    # See [the CSS3 HSL specification](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # Calculated from RGB where necessary via [this algorithm](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # @param color [Color]
    # @return [Number] between 0% and 100%
    # @raise [ArgumentError] if `color` isn't a color
    def lightness(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.lightness, ["%"])
    end

    # Returns the alpha component (opacity) of a color.
    # This is 1 unless otherwise specified.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def alpha(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.alpha)
    end
    alias_method :opacity, :alpha

    # Makes a color more opaque.
    # Takes a color and an amount between 0 and 1,
    # and returns a color with the opacity increased by that value.
    #
    # For example:
    #
    #     opacify(rgba(0, 0, 0, 0.5), 0.1) => rgba(0, 0, 0, 0.6)
    #     opacify(rgba(0, 0, 17, 0.8), 0.2) => #001
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0 and 1
    def opacify(color, amount)
      adjust(color, amount, :alpha, 0..1, :+)
    end
    alias_method :fade_in, :opacify

    # Makes a color more transparent.
    # Takes a color and an amount between 0 and 1,
    # and returns a color with the opacity decreased by that value.
    #
    # For example:
    #
    #     transparentize(rgba(0, 0, 0, 0.5), 0.1) => rgba(0, 0, 0, 0.4)
    #     transparentize(rgba(0, 0, 0, 0.8), 0.2) => rgba(0, 0, 0, 0.6)
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0 and 1
    def transparentize(color, amount)
      adjust(color, amount, :alpha, 0..1, :-)
    end
    alias_method :fade_out, :transparentize

    # Makes a color lighter.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the lightness increased by that value.
    #
    # For example:
    #
    #     lighten(hsl(0, 0%, 0%), 30%) => hsl(0, 0, 30)
    #     lighten(#800, 20%) => #e00
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def lighten(color, amount)
      adjust(color, amount, :lightness, 0..100, :+, "%")
    end

    # Makes a color darker.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the lightness decreased by that value.
    #
    # For example:
    #
    #     darken(hsl(25, 100%, 80%), 30%) => hsl(25, 100%, 50%)
    #     darken(#800, 20%) => #200
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def darken(color, amount)
      adjust(color, amount, :lightness, 0..100, :-, "%")
    end

    # Makes a color more saturated.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the saturation increased by that value.
    #
    # For example:
    #
    #     saturate(hsl(120, 30%, 90%), 20%) => hsl(120, 50%, 90%)
    #     saturate(#855, 20%) => #9e3f3f
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def saturate(color, amount)
      adjust(color, amount, :saturation, 0..100, :+, "%")
    end

    # Makes a color less saturated.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the saturation decreased by that value.
    #
    # For example:
    #
    #     desaturate(hsl(120, 30%, 90%), 20%) => hsl(120, 10%, 90%)
    #     desaturate(#855, 20%) => #726b6b
    #
    # @param color [Color]
    # @param amount [Number]
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def desaturate(color, amount)
      adjust(color, amount, :saturation, 0..100, :-, "%")
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
        raise ArgumentError.new("#{value.inspect} is not a unitless number")
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
      assert_type value, :Number
      Sass::Script::Number.new(yield(value.value), value.numerator_units, value.denominator_units)
    end

    def adjust(color, amount, attr, range, op, units = "")
      assert_type color, :Color
      assert_type amount, :Number
      unless range.include?(amount.value)
        raise ArgumentError.new("Amount #{amount} must be between #{range.first}#{units} and #{range.last}#{units}")
      end

      # TODO: is it worth restricting here,
      # or should we do so in the Color constructor itself,
      # and allow clipping in rgb() et al?
      color.with(attr => Haml::Util.restrict(
          color.send(attr).send(op, amount.value), range))
    end
  end
end
