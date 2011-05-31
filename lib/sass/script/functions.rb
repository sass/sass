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
  # *Note: These functions are described in more detail below.*
  #
  # ## RGB Functions
  #
  # \{#rgb rgb($red, $green, $blue)}
  # : Converts an `rgb(red, green, blue)` triplet into a color.
  #
  # \{#rgba rgba($red, $green, $blue, $alpha)}
  # : Converts an `rgba(red, green, blue, alpha)` quadruplet into a color.
  #
  # \{#rgba rgba($color, $alpha)}
  # : Adds an alpha layer to any color value.
  #
  # \{#red red($color)}
  # : Gets the red component of a color.
  #
  # \{#green green($color)}
  # : Gets the green component of a color.
  #
  # \{#blue blue($color)}
  # : Gets the blue component of a color.
  #
  # \{#mix mix($color-1, $color-2, \[$weight\])}
  # : Mixes two colors together.
  #
  # ## HSL Functions
  #
  # \{#hsl hsl($hue, $saturation, $lightness)}
  # : Converts an `hsl(hue, saturation, lightness)` triplet into a color.
  #
  # \{#hsla hsla($hue, $saturation, $lightness, $alpha)}
  # : Converts an `hsla(hue, saturation, lightness, alpha)` quadruplet into a color.
  #
  # \{#hue hue($color)}
  # : Gets the hue component of a color.
  #
  # \{#saturation saturation($color)}
  # : Gets the saturation component of a color.
  #
  # \{#lightness lightness($color)}
  # : Gets the lightness component of a color.
  #
  # \{#adjust_hue adjust-hue($color, $degrees)}
  # : Changes the hue of a color.
  #
  # \{#lighten lighten($color, $amount)}
  # : Makes a color lighter.
  #
  # \{#darken darken($color, $amount)}
  # : Makes a color darker.
  #
  # \{#saturate saturate($color, $amount)}
  # : Makes a color more saturated.
  #
  # \{#desaturate desaturate($color, $amount)}
  # : Makes a color less saturated.
  #
  # \{#grayscale grayscale($color)}
  # : Converts a color to grayscale.
  #
  # \{#complement complement($color)}
  # : Returns the complement of a color.
  #
  # \{#invert invert($color)}
  # : Returns the inverse of a color.
  #
  # ## Opacity Functions
  #
  # \{#alpha alpha($color)} / \{#opacity opacity($color)}
  # : Gets the alpha component (opacity) of a color.
  #
  # \{#rgba rgba($color, $alpha)}
  # : Add or change an alpha layer for any color value.
  #
  # \{#opacify opacify($color, $amount)} / \{#fade_in fade-in($color, $amount)}
  # : Makes a color more opaque.
  #
  # \{#transparentize transparentize($color, $amount)} / \{#fade_out fade-out($color, $amount)}
  # : Makes a color more transparent.
  #
  # ## Other Color Functions
  #
  # \{#adjust_color adjust-color($color, \[$red\], \[$green\], \[$blue\], \[$hue\], \[$saturation\], \[$lightness\], \[$alpha\]}
  # : Increase or decrease any of the components of a color.
  #
  # \{#scale_color scale-color($color, \[$red\], \[$green\], \[$blue\], \[$hue\], \[$saturation\], \[$lightness\], \[$alpha\]}
  # : Fluidly scale one or more components of a color.
  #
  # \{#change_color change-color($color, \[$red\], \[$green\], \[$blue\], \[$hue\], \[$saturation\], \[$lightness\], \[$alpha\]}
  # : Changes one or more properties of a color.
  #
  # ## String Functions
  #
  # \{#unquote unquote($string)}
  # : Removes the quotes from a string.
  #
  # \{#quote quote($string)}
  # : Adds quotes to a string.
  #
  # ## Number Functions
  #
  # \{#percentage percentage($value)}
  # : Converts a unitless number to a percentage.
  #
  # \{#round round($value)}
  # : Rounds a number to the nearest whole number.
  #
  # \{#ceil ceil($value)}
  # : Rounds a number up to the nearest whole number.
  #
  # \{#floor floor($value)}
  # : Rounds a number down to the nearest whole number.
  #
  # \{#abs abs($value)}
  # : Returns the absolute value of a number.
  #
  # ## List Functions {#list-functions}
  #
  # \{#length length($list)}
  # : Returns the length of a list.
  #
  # \{#nth nth($list, $n)}
  # : Returns a specific item in a list.
  #
  # \{#join join($list1, $list2, \[$separator\])}
  # : Joins together two lists into one.
  #
  # ## Introspection Functions
  #
  # \{#type_of type-of($value)}
  # : Returns the type of a value.
  #
  # \{#unit unit($number)}
  # : Returns the units associated with a number.
  #
  # \{#unitless unitless($number)}
  # : Returns whether a number has units or not.
  #
  # \{#comparable comparable($number-1, $number-2)}
  # : Returns whether two numbers can be added or compared.
  #
  # ## Miscellaneous Functions
  #
  # \{#if if($condition, $if-true, $if-false)}
  # : Returns one of two values, depending on whether or not a condition is true.
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
  #       declare :reverse, :args => [:string]
  #     end
  #
  # Calling {declare} tells Sass the argument names for your function.
  # If omitted, the function will still work, but will not be able to accept keyword arguments.
  # {declare} can also allow your function to take arbitrary keyword arguments.
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
    @signatures = {}

    # A class representing a Sass function signature.
    #
    # @attr args [Array<Symbol>] The names of the arguments to the function.
    # @attr var_args [Boolean] Whether the function takes a variable number of arguments.
    # @attr var_kwargs [Boolean] Whether the function takes an arbitrary set of keyword arguments.
    Signature = Struct.new(:args, :var_args, :var_kwargs)

    # Declare a Sass signature for a Ruby-defined function.
    # This includes the names of the arguments,
    # whether the function takes a variable number of arguments,
    # and whether the function takes an arbitrary set of keyword arguments.
    #
    # It's not necessary to declare a signature for a function.
    # However, without a signature it won't support keyword arguments.
    #
    # A single function can have multiple signatures declared
    # as long as each one takes a different number of arguments.
    # It's also possible to declare multiple signatures
    # that all take the same number of arguments,
    # but none of them but the first will be used
    # unless the user uses keyword arguments.
    #
    # @param method_name [Symbol] The name of the method
    #   whose signature is being declared.
    # @param args [Array<Symbol>] The names of the arguments for the function signature.
    # @option options :var_args [Boolean] (false)
    #   Whether the function accepts a variable number of (unnamed) arguments
    #   in addition to the named arguments.
    # @option options :var_kwargs [Boolean] (false)
    #   Whether the function accepts other keyword arguments
    #   in addition to those in `:args`.
    #   If this is true, the Ruby function will be passed a hash from strings
    #   to {Sass::Script::Literal}s as the last argument.
    #   In addition, if this is true and `:var_args` is not,
    #   Sass will ensure that the last argument passed is a hash.
    # 
    # @example
    #   declare :rgba, [:hex, :alpha]
    #   declare :rgba, [:red, :green, :blue, :alpha]
    #   declare :accepts_anything, [], :var_args => true, :var_kwargs => true
    #   declare :some_func, [:foo, :bar, :baz], :var_kwargs => true
    def self.declare(method_name, args, options = {})
      @signatures[method_name] ||= []
      @signatures[method_name] << Signature.new(
        args.map {|s| s.to_s},
        options[:var_args],
        options[:var_kwargs])
    end

    # Determine the correct signature for the number of arguments
    # passed in for a given function.
    # If no signatures match, the first signature is returned for error messaging.
    #
    # @param method_name [Symbol] The name of the Ruby function to be called.
    # @param arg_arity [Number] The number of unnamed arguments the function was passed.
    # @param kwarg_arity [Number] The number of keyword arguments the function was passed.
    #
    # @return [{Symbol => Object}, nil]
    #   The signature options for the matching signature,
    #   or nil if no signatures are declared for this function. See {declare}.
    def self.signature(method_name, arg_arity, kwarg_arity)
      return unless @signatures[method_name]
      @signatures[method_name].each do |signature|
        return signature if signature.args.size == arg_arity + kwarg_arity
        next unless signature.args.size < arg_arity + kwarg_arity

        # We have enough args.
        # Now we need to figure out which args are varargs
        # and if the signature allows them.
        t_arg_arity, t_kwarg_arity = arg_arity, kwarg_arity
        if signature.args.size > t_arg_arity
          # we transfer some kwargs arity to args arity
          # if it does not have enough args -- assuming the names will work out.
          t_kwarg_arity -= (signature.args.size - t_arg_arity)
          t_arg_arity = signature.args.size
        end

        if (  t_arg_arity == signature.args.size ||   t_arg_arity > signature.args.size && signature.var_args  ) &&
           (t_kwarg_arity == 0                   || t_kwarg_arity > 0                   && signature.var_kwargs)
          return signature
        end
      end
      @signatures[method_name].first
    end

    # The context in which methods in {Script::Functions} are evaluated.
    # That means that all instance methods of {EvaluationContext}
    # are available to use in functions.
    class EvaluationContext
      include Functions

      # The options hash for the {Sass::Engine} that is processing the function call
      #
      # @return [{Symbol => Object}]
      attr_reader :options

      # @param options [{Symbol => Object}] See \{#options}
      def initialize(options)
        @options = options
      end

      # Asserts that the type of a given SassScript value
      # is the expected type (designated by a symbol).
      #
      # Valid types are `:Bool`, `:Color`, `:Number`, and `:String`.
      # Note that `:String` will match both double-quoted strings
      # and unquoted identifiers.
      #
      # @example
      #   assert_type value, :String
      #   assert_type value, :Number
      # @param value [Sass::Script::Literal] A SassScript value
      # @param type [Symbol] The name of the type the value is expected to be
      # @param name [String, nil] The name of the argument.
      def assert_type(value, type, name = nil)
        return if value.is_a?(Sass::Script.const_get(type))
        err = "#{value.inspect} is not a #{type.to_s.downcase}"
        err = "$#{name}: " + err if name
        raise ArgumentError.new(err)
      end
    end

    class << self
      # Returns whether user function with a given name exists.
      #
      # @param function_name [String]
      # @return [Boolean]
      alias_method :callable?, :public_method_defined?

      private
      def include(*args)
        r = super
        # We have to re-include ourselves into EvaluationContext to work around
        # an icky Ruby restriction.
        EvaluationContext.send :include, self
        r
      end
    end

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
    declare :rgb, [:red, :green, :blue]

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
    declare :rgba, [:red, :green, :blue, :alpha]
    declare :rgba, [:color, :alpha]

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
    declare :hsl, [:hue, :saturation, :lightness]

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
    declare :hsla, [:hue, :saturation, :lightness, :alpha]

    # Returns the red component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def red(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.red)
    end
    declare :red, [:color]

    # Returns the green component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def green(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.green)
    end
    declare :green, [:color]

    # Returns the blue component of a color.
    #
    # @param color [Color]
    # @return [Number]
    # @raise [ArgumentError] If `color` isn't a color
    def blue(color)
      assert_type color, :Color
      Sass::Script::Number.new(color.blue)
    end
    declare :blue, [:color]

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
    declare :hue, [:color]

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
    declare :saturation, [:color]

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
    declare :lightness, [:color]

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
    declare :alpha, [:color]

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
    declare :opacity, [:color]

    # Makes a color more opaque.
    # Takes a color and an amount between 0 and 1,
    # and returns a color with the opacity increased by that value.
    #
    # @example
    #   opacify(rgba(0, 0, 0, 0.5), 0.1) => rgba(0, 0, 0, 0.6)
    #   opacify(rgba(0, 0, 17, 0.8), 0.2) => #001
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #transparentize
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0 and 1
    def opacify(color, amount)
      _adjust(color, amount, :alpha, 0..1, :+)
    end
    declare :opacify, [:color, :amount]

    alias_method :fade_in, :opacify
    declare :fade_in, [:color, :amount]

    # Makes a color more transparent.
    # Takes a color and an amount between 0 and 1,
    # and returns a color with the opacity decreased by that value.
    #
    # @example
    #   transparentize(rgba(0, 0, 0, 0.5), 0.1) => rgba(0, 0, 0, 0.4)
    #   transparentize(rgba(0, 0, 0, 0.8), 0.2) => rgba(0, 0, 0, 0.6)
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #opacify
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0 and 1
    def transparentize(color, amount)
      _adjust(color, amount, :alpha, 0..1, :-)
    end
    declare :transparentize, [:color, :amount]

    alias_method :fade_out, :transparentize
    declare :fade_out, [:color, :amount]

    # Makes a color lighter.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the lightness increased by that value.
    #
    # @example
    #   lighten(hsl(0, 0%, 0%), 30%) => hsl(0, 0, 30)
    #   lighten(#800, 20%) => #e00
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #darken
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def lighten(color, amount)
      _adjust(color, amount, :lightness, 0..100, :+, "%")
    end
    declare :lighten, [:color, :amount]

    # Makes a color darker.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the lightness decreased by that value.
    #
    # @example
    #   darken(hsl(25, 100%, 80%), 30%) => hsl(25, 100%, 50%)
    #   darken(#800, 20%) => #200
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #lighten
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def darken(color, amount)
      _adjust(color, amount, :lightness, 0..100, :-, "%")
    end
    declare :darken, [:color, :amount]

    # Makes a color more saturated.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the saturation increased by that value.
    #
    # @example
    #   saturate(hsl(120, 30%, 90%), 20%) => hsl(120, 50%, 90%)
    #   saturate(#855, 20%) => #9e3f3f
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #desaturate
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def saturate(color, amount)
      _adjust(color, amount, :saturation, 0..100, :+, "%")
    end
    declare :saturate, [:color, :amount]

    # Makes a color less saturated.
    # Takes a color and an amount between 0% and 100%,
    # and returns a color with the saturation decreased by that value.
    #
    # @example
    #   desaturate(hsl(120, 30%, 90%), 20%) => hsl(120, 10%, 90%)
    #   desaturate(#855, 20%) => #726b6b
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @see #saturate
    # @raise [ArgumentError] If `color` isn't a color,
    #   or `number` isn't a number between 0% and 100%
    def desaturate(color, amount)
      _adjust(color, amount, :saturation, 0..100, :-, "%")
    end
    declare :desaturate, [:color, :amount]

    # Changes the hue of a color while retaining the lightness and saturation.
    # Takes a color and a number of degrees (usually between -360deg and 360deg),
    # and returns a color with the hue rotated by that value.
    #
    # @example
    #   adjust-hue(hsl(120, 30%, 90%), 60deg) => hsl(180, 30%, 90%)
    #   adjust-hue(hsl(120, 30%, 90%), 060deg) => hsl(60, 30%, 90%)
    #   adjust-hue(#811, 45deg) => #886a11
    # @param color [Color]
    # @param amount [Number]
    # @return [Color]
    # @raise [ArgumentError] If `color` isn't a color, or `number` isn't a number
    def adjust_hue(color, degrees)
      assert_type color, :Color
      assert_type degrees, :Number
      color.with(:hue => color.hue + degrees.value)
    end
    declare :adjust_hue, [:color, :degrees]

    # Adjusts one or more properties of a color.
    # This can change the red, green, blue, hue, saturation, value, and alpha properties.
    # The properties are specified as keyword arguments,
    # and are added to or subtracted from the color's current value for that property.
    #
    # `$red`, `$green`, and `$blue` properties should be between 0 and 255.
    # `$saturation` and `$lightness` should be between 0% and 100%.
    # `$alpha` should be between 0 and 1.
    #
    # All properties are optional.
    # You can't specify both RGB properties (`$red`, `$green`, `$blue`)
    # and HSL properties (`$hue`, `$saturation`, `$value`) at the same time.
    #
    # @example
    #   adjust-color(#102030, $blue: 5) => #102035
    #   adjust-color(#102030, $red: -5, $blue: 5) => #0b2035
    #   adjust-color(hsl(25, 100%, 80%), $lightness: -30%, $alpha: -0.4) => hsla(25, 100%, 50%, 0.6)
    # @param color [Color]
    # @param red [Number]
    # @param green [Number]
    # @param blue [Number]
    # @param hue [Number]
    # @param saturation [Number]
    # @param lightness [Number]
    # @param alpha [Number]
    # @return [Color]
    # @raise [ArgumentError] if `color` is not a color,
    #   if any keyword argument is not a number,
    #   if any keyword argument is not in the legal range,
    #   if an unexpected keyword argument is given,
    #   or if both HSL and RGB properties are given.
    def adjust_color(color, kwargs)
      assert_type color, :Color
      with = Sass::Util.map_hash({
          "red" => [-255..255, ""],
          "green" => [-255..255, ""],
          "blue" => [-255..255, ""],
          "hue" => nil,
          "saturation" => [-100..100, "%"],
          "lightness" => [-100..100, "%"],
          "alpha" => [-1..1, ""]
        }) do |name, (range, units)|

        next unless val = kwargs.delete(name)
        assert_type val, :Number, name
        if range && !range.include?(val.value)
          raise ArgumentError.new("$#{name}: Amount #{val} must be between #{range.first}#{units} and #{range.last}#{units}")
        end
        adjusted = color.send(name) + val.value
        adjusted = [0, Sass::Util.restrict(adjusted, range)].max if range
        [name.to_sym, adjusted]
      end

      unless kwargs.empty?
        name, val = kwargs.to_a.first
        raise ArgumentError.new("Unknown argument $#{name} (#{val})")
      end

      color.with(with)
    end
    declare :adjust_color, [:color], :var_kwargs => true

    # Scales one or more properties of a color by a percentage value.
    # Unlike \{#adjust_color adjust-color}, which changes a color's properties by fixed amounts,
    # \{#scale_color scale-color} fluidly changes them based on how high or low they already are.
    # That means that lightening an already-light color with \{#scale_color scale-color}
    # won't change the lightness much,
    # but lightening a dark color by the same amount will change it more dramatically.
    # This has the benefit of making `scale-color($color, ...)` have a similar effect
    # regardless of what `$color` is.
    #
    # For example, the lightness of a color can be anywhere between 0 and 100.
    # If `scale-color($color, $lightness: 40%)` is called, the resulting color's lightness
    # will be 40% of the way between its original lightness and 100.
    # If `scale-color($color, $lightness: -40%)` is called instead,
    # the lightness will be 40% of the way between the original and 0.
    #
    # This can change the red, green, blue, saturation, value, and alpha properties.
    # The properties are specified as keyword arguments.
    # All arguments should be percentages between 0% and 100%.
    #
    # All properties are optional.
    # You can't specify both RGB properties (`$red`, `$green`, `$blue`)
    # and HSL properties (`$saturation`, `$value`) at the same time.
    #
    # @example
    #   scale-color(hsl(120, 70, 80), $lightness: 50%) => hsl(120, 70, 90)
    #   scale-color(rgb(200, 150, 170), $green: -40%, $blue: 70%) => rgb(200, 90, 229)
    #   scale-color(hsl(200, 70, 80), $saturation: -90%, $alpha: -30%) => hsla(200, 7, 80, 0.7)
    # @param color [Color]
    # @param red [Number]
    # @param green [Number]
    # @param blue [Number]
    # @param saturation [Number]
    # @param lightness [Number]
    # @param alpha [Number]
    # @return [Color]
    # @raise [ArgumentError] if `color` is not a color,
    #   if any keyword argument is not a percentage between 0% and 100%,
    #   if an unexpected keyword argument is given,
    #   or if both HSL and RGB properties are given.
    def scale_color(color, kwargs)
      assert_type color, :Color
      with = Sass::Util.map_hash({
          "red" => 255,
          "green" => 255,
          "blue" => 255,
          "saturation" => 100,
          "lightness" => 100,
          "alpha" => 1
        }) do |name, max|

        next unless val = kwargs.delete(name)
        assert_type val, :Number, name
        if !(val.numerator_units == ['%'] && val.denominator_units.empty?)
          raise ArgumentError.new("$#{name}: Amount #{val} must be a % (e.g. #{val.value}%)")
        elsif !(-100..100).include?(val.value)
          raise ArgumentError.new("$#{name}: Amount #{val} must be between -100% and 100%")
        end

        current = color.send(name)
        scale = val.value/100.0
        diff = scale > 0 ? max - current : current
        [name.to_sym, current + diff*scale]
      end

      unless kwargs.empty?
        name, val = kwargs.to_a.first
        raise ArgumentError.new("Unknown argument $#{name} (#{val})")
      end

      color.with(with)
    end
    declare :scale_color, [:color], :var_kwargs => true

    # Changes one or more properties of a color.
    # This can change the red, green, blue, hue, saturation, value, and alpha properties.
    # The properties are specified as keyword arguments,
    # and replace the color's current value for that property.
    #
    # `$red`, `$green`, and `$blue` properties should be between 0 and 255.
    # `$saturation` and `$lightness` should be between 0% and 100%.
    # `$alpha` should be between 0 and 1.
    #
    # All properties are optional.
    # You can't specify both RGB properties (`$red`, `$green`, `$blue`)
    # and HSL properties (`$hue`, `$saturation`, `$value`) at the same time.
    #
    # @example
    #   change-color(#102030, $blue: 5) => #102005
    #   change-color(#102030, $red: 120, $blue: 5) => #782005
    #   change-color(hsl(25, 100%, 80%), $lightness: 40%, $alpha: 0.8) => hsla(25, 100%, 40%, 0.8)
    # @param color [Color]
    # @param red [Number]
    # @param green [Number]
    # @param blue [Number]
    # @param hue [Number]
    # @param saturation [Number]
    # @param lightness [Number]
    # @param alpha [Number]
    # @return [Color]
    # @raise [ArgumentError] if `color` is not a color,
    #   if any keyword argument is not a number,
    #   if any keyword argument is not in the legal range,
    #   if an unexpected keyword argument is given,
    #   or if both HSL and RGB properties are given.
    def change_color(color, kwargs)
      assert_type color, :Color
      with = Sass::Util.map_hash(%w[red green blue hue saturation lightness alpha]) do |name, max|
        next unless val = kwargs.delete(name)
        assert_type val, :Number, name
        [name.to_sym, val.value]
      end

      unless kwargs.empty?
        name, val = kwargs.to_a.first
        raise ArgumentError.new("Unknown argument $#{name} (#{val})")
      end

      color.with(with)
    end
    declare :change_color, [:color], :var_kwargs => true

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
    # @example
    #   mix(#f00, #00f) => #7f007f
    #   mix(#f00, #00f, 25%) => #3f00bf
    #   mix(rgba(255, 0, 0, 0.5), #00f) => rgba(63, 0, 191, 0.75)
    # @overload mix(color1, color2, weight: 50%)
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
    declare :mix, [:color_1, :color_2]
    declare :mix, [:color_1, :color_2, :weight]

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
    declare :grayscale, [:color]

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
    declare :complement, [:color]

    # Returns the inverse (negative) of a color.
    # The red, green, and blue values are inverted, while the opacity is left alone.
    #
    # @param color [Color]
    # @return [Color]
    # @raise [ArgumentError] if `color` isn't a color
    def invert(color)
      assert_type color, :Color
      color.with(
        :red => (255 - color.red),
        :green => (255 - color.green),
        :blue => (255 - color.blue))
    end

    # Removes quotes from a string if the string is quoted,
    # or returns the same string if it's not.
    #
    # @param string [String]
    # @return [String]
    # @raise [ArgumentError] if `string` isn't a string
    # @see #quote
    # @example
    #   unquote("foo") => foo
    #   unquote(foo) => foo
    def unquote(string)
      if string.is_a?(Sass::Script::String)
        Sass::Script::String.new(string.value, :identifier)
      else
        string
      end
    end
    declare :unquote, [:string]

    # Add quotes to a string if the string isn't quoted,
    # or returns the same string if it is.
    #
    # @param string [String]
    # @return [String]
    # @raise [ArgumentError] if `string` isn't a string
    # @see #unquote
    # @example
    #   quote("foo") => "foo"
    #   quote(foo) => "foo"
    def quote(string)
      assert_type string, :String
      Sass::Script::String.new(string.value, :string)
    end
    declare :quote, [:string]

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
    declare :type_of, [:value]

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
    declare :unit, [:number]

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
    declare :unitless, [:number]

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
    declare :comparable, [:number_1, :number_2]

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
    declare :percentage, [:value]

    # Rounds a number to the nearest whole number.
    #
    # @example
    #   round(10.4px) => 10px
    #   round(10.6px) => 11px
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [ArgumentError] if `value` isn't a number
    def round(value)
      numeric_transformation(value) {|n| n.round}
    end
    declare :round, [:value]

    # Rounds a number up to the nearest whole number.
    #
    # @example
    #   ciel(10.4px) => 11px
    #   ciel(10.6px) => 11px
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [ArgumentError] if `value` isn't a number
    def ceil(value)
      numeric_transformation(value) {|n| n.ceil}
    end
    declare :ceil, [:value]

    # Rounds down to the nearest whole number.
    #
    # @example
    #   floor(10.4px) => 10px
    #   floor(10.6px) => 10px
    # @param value [Number] The number
    # @return [Number] The rounded number
    # @raise [ArgumentError] if `value` isn't a number
    def floor(value)
      numeric_transformation(value) {|n| n.floor}
    end
    declare :floor, [:value]

    # Finds the absolute value of a number.
    #
    # @example
    #   abs(10px) => 10px
    #   abs(-10px) => 10px
    # @param value [Number] The number
    # @return [Number] The absolute value
    # @raise [ArgumentError] if `value` isn't a number
    def abs(value)
      numeric_transformation(value) {|n| n.abs}
    end
    declare :abs, [:value]

    # Return the length of a list.
    #
    # @example
    #   length(10px) => 1
    #   length(10px 20px 30px) => 3
    # @param list [Literal] The list
    # @return [Number] The length
    def length(list)
      Sass::Script::Number.new(list.to_a.size)
    end
    declare :length, [:list]

    # Gets the nth item in a list.
    #
    # Note that unlike some languages, the first item in a Sass list is number 1,
    # the second number 2, and so forth.
    #
    # @example
    #   nth(10px 20px 30px, 1) => 10px
    #   nth((Helvetica, Arial, sans-serif), 3) => sans-serif
    # @param list [Literal] The list
    # @param n [Number] The index into the list
    # @return [Literal] The nth item in the list
    # @raise [ArgumentError] If `n` isn't an integer between 1 and the list's length.
    def nth(list, n)
      assert_type n, :Number
      if !n.int?
        raise ArgumentError.new("List index #{n} must be an integer")
      elsif n.to_i < 1
        raise ArgumentError.new("List index #{n} must be greater than or equal to 1")
      elsif list.to_a.size == 0
        raise ArgumentError.new("List index is #{n} but list has no items")
      elsif n.to_i > (size = list.to_a.size)
        raise ArgumentError.new("List index is #{n} but list is only #{size} item#{'s' if size != 1} long")
      end

      list.to_a[n.to_i - 1]
    end
    declare :nth, [:list, :n]

    # Joins together two lists into a new list.
    #
    # Unless the `$separator` argument is passed,
    # if one list is comma-separated and one is space-separated,
    # the first parameter's separator is used for the resulting list.
    # If the lists have only one item each, spaces are used for the resulting list.
    #
    # @example
    #   join(10px 20px, 30px 40px) => 10px 20px 30px 40px
    #   join((blue, red), (#abc, #def)) => blue, red, #abc, #def
    #   join(10px, 20px) => 10px 20px
    #   join(10px, 20px, comma) => 10px, 20px
    #   join((blue, red), (#abc, #def), space) => blue red #abc #def
    # @overload join(list1, list2, separator: auto)
    #   @param list1 [Literal] The first list to join
    #   @param list2 [Literal] The second list to join
    #   @param separator [String] How the list separator (comma or space) should be determined.
    #     If this is `comma` or `space`, that is always the separator;
    #     if this is `auto` (the default), the separator is determined as explained above.
    def join(list1, list2, separator = Sass::Script::String.new("auto"))
      assert_type separator, :String
      unless %w[auto space comma].include?(separator.value)
        raise ArgumentError.new("Separator name must be space, comma, or auto")
      end
      sep1 = list1.separator if list1.is_a?(Sass::Script::List) && !list1.value.empty?
      sep2 = list2.separator if list2.is_a?(Sass::Script::List) && !list2.value.empty?
      Sass::Script::List.new(
        list1.to_a + list2.to_a,
        if separator.value == 'auto'
          sep1 || sep2 || :space
        else
          separator.value.to_sym
        end)
    end
    declare :join, [:list1, :list2]
    declare :join, [:list1, :list2, :separator]

    # Appends a single value onto the end of a list.
    #
    # Unless the `$separator` argument is passed,
    # if the list has only one item,
    # the resulting list will be space-separated.
    #
    # @example
    #   append(10px 20px, 30px) => 10px 20px 30px
    #   append((blue, red), green) => blue, red, green
    #   append(10px 20px, 30px 40px) => 10px 20px (30px 40px)
    #   join(10px, 20px, comma) => 10px, 20px
    #   join((blue, red), green, space) => blue red green
    # @overload join(list, val, separator: auto)
    #   @param list1 [Literal] The first list to join
    #   @param list2 [Literal] The second list to join
    #   @param separator [String] How the list separator (comma or space) should be determined.
    #     If this is `comma` or `space`, that is always the separator;
    #     if this is `auto` (the default), the separator is determined as explained above.
    def append(list, val, separator = Sass::Script::String.new("auto"))
      assert_type separator, :String
      unless %w[auto space comma].include?(separator.value)
        raise ArgumentError.new("Separator name must be space, comma, or auto")
      end
      sep = list.separator if list.is_a?(Sass::Script::List)
      Sass::Script::List.new(
        list.to_a + [val],
        if separator.value == 'auto'
          sep || :space
        else
          separator.value.to_sym
        end)
    end
    declare :append, [:list, :val]
    declare :append, [:list, :val, :separator]

    # Combines several lists into a single comma separated list
    # space separated lists.
    #
    # The length of the resulting list is the length of the
    # shortest list.
    #
    # @example
    #   zip(1px 1px 3px, solid dashed solid, red green blue)
    #   => 1px solid red, 1px dashed green, 3px solid blue
    def zip(*lists)
      length = nil
      values = []
      lists.each do |list|
        assert_type list, :List
        values << list.value.dup
        length = length.nil? ? list.value.length : [length, list.value.length].min
      end
      values.each do |value|
        value.slice!(length)
      end
      new_list_value = values.first.zip(*values[1..-1])
      List.new(new_list_value.map{|list| List.new(list, :space)}, :comma)
    end
    declare :zip, [], :var_args => true


    # Returns the position of the given value within the given
    # list. If not found, returns false.
    #
    # @example
    #   index(1px solid red, solid) => 2
    #   index(1px solid red, dashed) => false
    def index(list, value)
      assert_type list, :List
      index = list.value.index {|e| e.eq(value).to_bool }
      if index
        Number.new(index + 1)
      else
        Bool.new(false)
      end
    end
    declare :index, [:list, :value]

    # Returns one of two values based on the truth value of the first argument.
    #
    # @example
    #   if(true, 1px, 2px) => 1px
    #   if(false, 1px, 2px) => 2px
    # @param condition [Bool] Whether the first or second value will be returned.
    # @param if_true [Literal] The value that will be returned if `$condition` is true.
    # @param if_false [Literal] The value that will be returned if `$condition` is false.
    def if(condition, if_true, if_false)
      if condition.to_bool
        if_true
      else
        if_false
      end
    end
    declare :if, [:condition, :if_true, :if_false]

    private

    # This method implements the pattern of transforming a numeric value into
    # another numeric value with the same units.
    # It yields a number to a block to perform the operation and return a number
    def numeric_transformation(value)
      assert_type value, :Number
      Sass::Script::Number.new(yield(value.value), value.numerator_units, value.denominator_units)
    end

    def _adjust(color, amount, attr, range, op, units = "")
      assert_type color, :Color
      assert_type amount, :Number
      unless range.include?(amount.value)
        raise ArgumentError.new("Amount #{amount} must be between #{range.first}#{units} and #{range.last}#{units}")
      end

      # TODO: is it worth restricting here,
      # or should we do so in the Color constructor itself,
      # and allow clipping in rgb() et al?
      color.with(attr => Sass::Util.restrict(
          color.send(attr).send(op, amount.value), range))
    end
  end
end
