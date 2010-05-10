module Sass::Script
  # Methods in this module are accessible from the SassScript context.
  # For example, you can write
  #
  #     $color = hsl(120deg, 100%, 50%)
  #
  # and it will call {Sass::Script::Functions#hsl}.
  #
  # The following functions are provided:
  #
  # ## RGB Functions
  #
  # \{#rgb}
  # : Converts an `rgb(red, green, blue)` triplet into a color.
  #
  # \{#rgba}
  # : Converts an `rgba(red, green, blue, alpha)` quadruplet into a color.
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
  # \{#mix}
  # : Mixes two colors together.
  #
  # ## HSL Functions
  #
  # \{#hsl}
  # : Converts an `hsl(hue, saturation, lightness)` triplet into a color.
  #
  # \{#hsla}
  # : Converts an `hsla(hue, saturation, lightness, alpha)` quadruplet into a color.
  #
  # \{#hue}
  # : Gets the hue component of a color.
  #
  # \{#saturation}
  # : Gets the saturation component of a color.
  #
  # \{#lightness}
  # : Gets the lightness component of a color.
  #
  # \{#adjust_hue #adjust-hue}
  # : Changes the hue of a color.
  #
  # \{#lighten}
  # : Makes a color lighter.
  #
  # \{#darken}
  # : Makes a color darker.
  #
  # \{#saturate}
  # : Makes a color more saturated.
  #
  # \{#desaturate}
  # : Makes a color less saturated.
  #
  # \{#grayscale}
  # : Converts a color to grayscale.
  #
  # \{#complement}
  # : Returns the complement of a color.
  #
  # ## Opacity Functions
  #
  # \{#alpha} / \{#opacity}
  # : Gets the alpha component (opacity) of a color.
  #
  # \{#rgba}
  # : Sets the alpha component of a color.
  #
  # \{#opacify} / \{#fade_in #fade-in}
  # : Makes a color more opaque.
  #
  # \{#transparentize} / \{#fade_out #fade-out}
  # : Makes a color more transparent.
  #
  # ## String Functions
  #
  # \{#unquote}
  # : Removes the quotes from a string.
  #
  # \{#quote}
  # : Adds quotes to a string.
  #
  # ## Number Functions
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
  # ## Introspection Functions
  #
  # \{#type_of}
  # : Returns the type of a value.
  #
  # \{#unit}
  # : Returns the units associated with a number.
  #
  # \{#unitless}
  # : Returns whether a number has units or not.
  #
  # \{#comparable}
  # : Returns whether two numbers can be added or compared.
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
  # This is generally a bad idea;
  # since Sass files are by default only compiled once,
  # dynamic code is not a great fit.
  #
  # If you really, really need to compile Sass on each request,
  # first make sure you have adequate caching set up.
  # Then you can use {Sass::Engine} to render the code,
  # using the {file:SASS_REFERENCE.md#custom-option `options` parameter}
  # to pass in data that {EvaluationContext#options can be accessed}
  # from your Sass functions.
  #
  # Within one of the functions in this module,
  # methods of {EvaluationContext} can be used.
  #
  # ### Caveats
  #
  # When creating new {Literal} objects within functions,
  # be aware that it's not safe to call {Literal#to_s #to_s}
  # (or other methods that use the string representation)
  # on those objects without first setting {Node#options= the #options attribute}.
  module Functions
    # The context in which methods in {Script::Functions} are evaluated.
    # That means that all instance methods of {EvaluationContext}
    # are available to use in functions.
    class EvaluationContext
      # The options hash for the {Sass::Engine} that is processing the function call
      #
      # @return [{Symbol => Object}]
      attr_reader :options

      # @param options [{Symbol => Object}] See \{#options}
      def initialize(options)
        @options = options

        # We need to include this individually in each instance
        # because of an icky Ruby restriction
        class << self; include Sass::Script::Functions; end
      end

      # Asserts that the type of a given SassScript value
      # is the expected type (designated by a symbol).
      # For example:
      #
      #     assert_type value, :String
      #     assert_type value, :Number
      #
      # Valid types are `:Bool`, `:Color`, `:Number`, and `:String`.
      # Note that `:String` will match both double-quoted strings
      # and unquoted identifiers.
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
    #
    # @param red [Number]
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    # @param green [Number]
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    # @param blue [Number]
    #   A number between 0 and 255 inclusive,
    #   or between 0% and 100% inclusive
    # @see #rgba
    # @return [Color]
    def rgb(red, green, blue)
      assert_type red, :Number
      assert_type green, :Number
      assert_type blue, :Number

      Color.new([red, green, blue].map do |c|
          v = c.value
          if c.numerator_units == ["%"] && c.denominator_units.empty?
            next v * 255 / 100.0 if (0..100).include?(v)
            raise ArgumentError.new("Color value #{c} must be between 0% and 100% inclusive")
          else
            next v if (0..255).include?(v)
            raise ArgumentError.new("Color value #{v} must be between 0 and 255 inclusive")
          end
        end)
    end

    # @see #rgb
    # @overload rgba(red, green, blue, alpha)
    #   Creates a {Color} object from red, green, and blue values,
    #   as well as an alpha channel indicating opacity.
    #
    #   @param red [Number]
    #     A number between 0 and 255 inclusive
    #   @param green [Number]
    #     A number between 0 and 255 inclusive
    #   @param blue [Number]
    #     A number between 0 and 255 inclusive
    #   @param alpha [Number]
    #     A number between 0 and 1
    #   @return [Color]
    #
    # @overload rgba(color, alpha)
    #   Sets the opacity of a color.
    #
    #   @example
    #     rgba(#102030, 0.5) => rgba(16, 32, 48, 0.5)
    #     rgba(blue, 0.2)    => rgba(0, 0, 255, 0.2)
    #
    #   @param color [Color]
    #   @param alpha [Number]
    #     A number between 0 and 1
    #   @return [Color]
    def rgba(*args)
      case args.size
      when 2
        color, alpha = args

        assert_type color, :Color
        assert_type alpha, :Number

        unless (0..1).include?(alpha.value)
          raise ArgumentError.new("Alpha channel #{alpha.value} must be between 0 and 1 inclusive")
        end

        color.with(:alpha => alpha.value)
      when 4
        red, green, blue, alpha = args
        rgba(rgb(red, green, blue), alpha)
      else
        raise ArgumentError.new("wrong number of arguments (#{args.size} for 4)")
      end
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
    # @see #hsla
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
    # @see #hsl
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
    # @return [Number] between 0deg and 360deg
    # @see #adjust_hue
    # @raise [ArgumentError] if `color` isn't a color
    def hue(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.hue, ["deg"])
    end

    # Returns the saturation component of a color.
    #
    # See [the CSS3 HSL specification](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # Calculated from RGB where necessary via [this algorithm](http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV).
    #
    # @param color [Color]
    # @return [Number] between 0% and 100%
    # @see #saturate
    # @see #desaturate
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
    # @see #lighten
    # @see #darken
    # @raise [ArgumentError] if `color` isn't a color
    def lightness(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.lightness, ["%"])
    end

    # Returns the alpha component (opacity) of a color.
    # This is 1 unless otherwise specified.
    #
    # This function also supports the proprietary Microsoft
    # `alpha(opacity=20)` syntax.
    #
    # @overload def alpha(color)
    # @param color [Color]
    # @return [Number]
    # @see #opacify
    # @see #transparentize
    # @raise [ArgumentError] If `color` isn't a color
    def alpha(*args)
      if args.all? do |a|
          a.is_a?(Sass::Script::String) && a.type == :identifier &&
            a.value =~ /^[a-zA-Z]+\s*=/
        end
        # Support the proprietary MS alpha() function
        return Sass::Script::String.new("alpha(#{args.map {|a| a.to_s}.join(", ")})")
      end

      opacity(*args)
    end

    # Returns the alpha component (opacity) of a color.
    # This is 1 unless otherwise specified.
    #
    # @param color [Color]
    # @return [Number]
    # @see #opacify
    # @see #transparentize
    # @raise [ArgumentError] If `color` isn't a color
    def opacity(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.alpha)
    end

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
    # @return [Color]
    # @see #transparentize
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
    # @return [Color]
    # @see #opacify
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
    # @return [Color]
    # @see #darken
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
    # @return [Color]
    # @see #lighten
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
    # @return [Color]
    # @see #desaturate
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
    # @return [Color]
    # @see #saturate
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def desaturate(color, amount)
      adjust(color, amount, :saturation, 0..100, :-, "%")
    end

    # Changes the hue of a color while retaining the lightness and saturation.
    # Takes a color and a number of degrees (usually between -360deg and 360deg),
    # and returns a color with the hue rotated by that value.
    #
    # For example:
    #
    #     adjust-hue(hsl(120, 30%, 90%), 60deg) => hsl(180, 30%, 90%)
    #     adjust-hue(hsl(120, 30%, 90%), 060deg) => hsl(60, 30%, 90%)
    #     adjust-hue(#811, 45deg) => #886a11
    #
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @raise [ArgumentError] If `color` isn't a color, or `number` isn't a number
    def adjust_hue(color, degrees)
      assert_type color, :Color
      assert_type degrees, :Number
      color.with(:hue => color.hue + degrees.value)
    end

    # Mixes together two colors.
    # Specifically, takes the average of each of the RGB components,
    # optionally weighted by the given percentage.
    # The opacity of the colors is also considered when weighting the components.
    #
    # The weight specifies the amount of the first color that should be included
    # in the returned color.
    # The default, 50%, means that half the first color
    # and half the second color should be used.
    # 25% means that a quarter of the first color
    # and three quarters of the second color should be used.
    #
    # For example:
    #
    #     mix(#f00, #00f) => #7f007f
    #     mix(#f00, #00f, 25%) => #3f00bf
    #     mix(rgba(255, 0, 0, 0.5), #00f) => rgba(63, 0, 191, 0.75)
    #
    # @overload mix(color1, color2, weight = 50%)
    #   @param color1 [Color]
    #   @param color2 [Color]
    #   @param weight [Number] between 0% and 100%
    #   @return [Color]
    #   @raise [ArgumentError] if `color1` or `color2` aren't colors,
    #     or `weight` isn't a number between 0% and 100%
    def mix(color1, color2, weight = Number.new(50))
      assert_type color1, :Color
      assert_type color2, :Color
      assert_type weight, :Number

      unless (0..100).include?(weight.value)
        raise ArgumentError.new("Weight #{weight} must be between 0% and 100%")
      end

      # This algorithm factors in both the user-provided weight
      # and the difference between the alpha values of the two colors
      # to decide how to perform the weighted average of the two RGB values.
      #
      # It works by first normalizing both parameters to be within [-1, 1],
      # where 1 indicates "only use color1", -1 indicates "only use color 0",
      # and all values in between indicated a proportionately weighted average.
      #
      # Once we have the normalized variables w and a,
      # we apply the formula (w + a)/(1 + w*a)
      # to get the combined weight (in [-1, 1]) of color1.
      # This formula has two especially nice properties:
      #
      #   * When either w or a are -1 or 1, the combined weight is also that number
      #     (cases where w * a == -1 are undefined, and handled as a special case).
      #
      #   * When a is 0, the combined weight is w, and vice versa
      #
      # Finally, the weight of color1 is renormalized to be within [0, 1]
      # and the weight of color2 is given by 1 minus the weight of color1.
      p = weight.value/100.0
      w = p*2 - 1
      a = color1.alpha - color2.alpha

      w1 = (((w * a == -1) ? w : (w + a)/(1 + w*a)) + 1)/2.0
      w2 = 1 - w1

      rgb = color1.rgb.zip(color2.rgb).map {|v1, v2| v1*w1 + v2*w2}
      alpha = color1.alpha*p + color2.alpha*(1-p)
      Color.new(rgb + [alpha])
    end

    # Converts a color to grayscale.
    # This is identical to `desaturate(color, 100%)`.
    #
    # @param color [Color]
    # @return [Color]
    # @raise [ArgumentError] if `color` isn't a color
    # @see #desaturate
    def grayscale(color)
      desaturate color, Number.new(100)
    end

    # Returns the complement of a color.
    # This is identical to `adjust-hue(color, 180deg)`.
    #
    # @param color [Color]
    # @return [Color]
    # @raise [ArgumentError] if `color` isn't a color
    # @see #adjust_hue #adjust-hue
    def complement(color)
      adjust_hue color, Number.new(180)
    end

    # Removes quotes from a string if the string is quoted,
    # or returns the same string if it's not.
    #
    # @param str [String]
    # @return [String]
    # @raise [ArgumentError] if `str` isn't a string
    # @see #quote
    # @example
    # unquote("foo") => foo
    # unquote(foo) => foo
    def unquote(str)
      assert_type str, :String
      Sass::Script::String.new(str.value, :identifier)
    end

    # Add quotes to a string if the string isn't quoted,
    # or returns the same string if it is.
    #
    # @param str [String]
    # @return [String]
    # @raise [ArgumentError] if `str` isn't a string
    # @see #unquote
    # @example
    # quote("foo") => "foo"
    # quote(foo) => "foo"
    def quote(str)
      assert_type str, :String
      Sass::Script::String.new(str.value, :string)
    end

    # Inspects the type of the argument, returning it as an unquoted string.
    # For example:
    #
    #     type-of(100px)  => number
    #     type-of(asdf)   => string
    #     type-of("asdf") => string
    #     type-of(true)   => bool
    #     type-of(#fff)   => color
    #     type-of(blue)   => color
    #
    # @param obj [Literal] The object to inspect
    # @return [String] The unquoted string name of the literal's type
    def type_of(obj)
      Sass::Script::String.new(obj.class.name.gsub(/Sass::Script::/,'').downcase)
    end

    # Inspects the unit of the number, returning it as a quoted string.
    # Complex units are sorted in alphabetical order by numerator and denominator.
    # For example:
    #
    #     unit(100) => ""
    #     unit(100px) => "px"
    #     unit(3em) => "em"
    #     unit(10px * 5em) => "em*px"
    #     unit(10px * 5em / 30cm / 1rem) => "em*px/cm*rem"
    #
    # @param number [Literal] The number to inspect
    # @return [String] The unit(s) of the number
    # @raise [ArgumentError] if `number` isn't a number
    def unit(number)
      assert_type number, :Number
      Sass::Script::String.new(number.unit_str, :string)
    end

    # Inspects the unit of the number, returning a boolean indicating if it is unitless.
    # For example:
    #
    #     unitless(100) => true
    #     unitless(100px) => false
    #
    # @param number [Literal] The number to inspect
    # @return [Bool] Whether or not the number is unitless
    # @raise [ArgumentError] if `number` isn't a number
    def unitless(number)
      assert_type number, :Number
      Sass::Script::Bool.new(number.unitless?)
    end

    # Returns true if two numbers are similar enough to be added, subtracted, or compared.
    # For example:
    #
    #     comparable(2px, 1px) => true
    #     comparable(100px, 3em) => false
    #     comparable(10cm, 3mm) => true
    #
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
