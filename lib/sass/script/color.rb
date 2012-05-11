require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a CSS color.
  #
  # A color may be represented internally as RGBA, HSLA, or both.
  # It's originally represented as whatever its input is;
  # if it's created with RGB values, it's represented as RGBA,
  # and if it's created with HSL values, it's represented as HSLA.
  # Once a property is accessed that requires the other representation --
  # for example, \{#red} for an HSL color --
  # that component is calculated and cached.
  #
  # The alpha channel of a color is independent of its RGB or HSL representation.
  # It's always stored, as 1 if nothing else is specified.
  # If only the alpha channel is modified using \{#with},
  # the cached RGB and HSL values are retained.
  class Color < Literal
    class << self; include Sass::Util; end

    # A hash from color names to `[red, green, blue]` value arrays.
    COLOR_NAMES = map_vals({
        'aliceblue' => 0xf0f8ff,
        'antiquewhite' => 0xfaebd7,
        'aqua' => 0x00ffff,
        'aquamarine' => 0x7fffd4,
        'azure' => 0xf0ffff,
        'beige' => 0xf5f5dc,
        'bisque' => 0xffe4c4,
        'black' => 0x000000,
        'blanchedalmond' => 0xffebcd,
        'blue' => 0x0000ff,
        'blueviolet' => 0x8a2be2,
        'brown' => 0xa52a2a,
        'burlywood' => 0xdeb887,
        'cadetblue' => 0x5f9ea0,
        'chartreuse' => 0x7fff00,
        'chocolate' => 0xd2691e,
        'coral' => 0xff7f50,
        'cornflowerblue' => 0x6495ed,
        'cornsilk' => 0xfff8dc,
        'crimson' => 0xdc143c,
        'cyan' => 0x00ffff,
        'darkblue' => 0x00008b,
        'darkcyan' => 0x008b8b,
        'darkgoldenrod' => 0xb8860b,
        'darkgray' => 0xa9a9a9,
        'darkgrey' => 0xa9a9a9,
        'darkgreen' => 0x006400,
        'darkkhaki' => 0xbdb76b,
        'darkmagenta' => 0x8b008b,
        'darkolivegreen' => 0x556b2f,
        'darkorange' => 0xff8c00,
        'darkorchid' => 0x9932cc,
        'darkred' => 0x8b0000,
        'darksalmon' => 0xe9967a,
        'darkseagreen' => 0x8fbc8f,
        'darkslateblue' => 0x483d8b,
        'darkslategray' => 0x2f4f4f,
        'darkslategrey' => 0x2f4f4f,
        'darkturquoise' => 0x00ced1,
        'darkviolet' => 0x9400d3,
        'deeppink' => 0xff1493,
        'deepskyblue' => 0x00bfff,
        'dimgray' => 0x696969,
        'dimgrey' => 0x696969,
        'dodgerblue' => 0x1e90ff,
        'firebrick' => 0xb22222,
        'floralwhite' => 0xfffaf0,
        'forestgreen' => 0x228b22,
        'fuchsia' => 0xff00ff,
        'gainsboro' => 0xdcdcdc,
        'ghostwhite' => 0xf8f8ff,
        'gold' => 0xffd700,
        'goldenrod' => 0xdaa520,
        'gray' => 0x808080,
        'green' => 0x008000,
        'greenyellow' => 0xadff2f,
        'honeydew' => 0xf0fff0,
        'hotpink' => 0xff69b4,
        'indianred' => 0xcd5c5c,
        'indigo' => 0x4b0082,
        'ivory' => 0xfffff0,
        'khaki' => 0xf0e68c,
        'lavender' => 0xe6e6fa,
        'lavenderblush' => 0xfff0f5,
        'lawngreen' => 0x7cfc00,
        'lemonchiffon' => 0xfffacd,
        'lightblue' => 0xadd8e6,
        'lightcoral' => 0xf08080,
        'lightcyan' => 0xe0ffff,
        'lightgoldenrodyellow' => 0xfafad2,
        'lightgreen' => 0x90ee90,
        'lightgray' => 0xd3d3d3,
        'lightgrey' => 0xd3d3d3,
        'lightpink' => 0xffb6c1,
        'lightsalmon' => 0xffa07a,
        'lightseagreen' => 0x20b2aa,
        'lightskyblue' => 0x87cefa,
        'lightslategray' => 0x778899,
        'lightslategrey' => 0x778899,
        'lightsteelblue' => 0xb0c4de,
        'lightyellow' => 0xffffe0,
        'lime' => 0x00ff00,
        'limegreen' => 0x32cd32,
        'linen' => 0xfaf0e6,
        'magenta' => 0xff00ff,
        'maroon' => 0x800000,
        'mediumaquamarine' => 0x66cdaa,
        'mediumblue' => 0x0000cd,
        'mediumorchid' => 0xba55d3,
        'mediumpurple' => 0x9370db,
        'mediumseagreen' => 0x3cb371,
        'mediumslateblue' => 0x7b68ee,
        'mediumspringgreen' => 0x00fa9a,
        'mediumturquoise' => 0x48d1cc,
        'mediumvioletred' => 0xc71585,
        'midnightblue' => 0x191970,
        'mintcream' => 0xf5fffa,
        'mistyrose' => 0xffe4e1,
        'moccasin' => 0xffe4b5,
        'navajowhite' => 0xffdead,
        'navy' => 0x000080,
        'oldlace' => 0xfdf5e6,
        'olive' => 0x808000,
        'olivedrab' => 0x6b8e23,
        'orange' => 0xffa500,
        'orangered' => 0xff4500,
        'orchid' => 0xda70d6,
        'palegoldenrod' => 0xeee8aa,
        'palegreen' => 0x98fb98,
        'paleturquoise' => 0xafeeee,
        'palevioletred' => 0xdb7093,
        'papayawhip' => 0xffefd5,
        'peachpuff' => 0xffdab9,
        'peru' => 0xcd853f,
        'pink' => 0xffc0cb,
        'plum' => 0xdda0dd,
        'powderblue' => 0xb0e0e6,
        'purple' => 0x800080,
        'red' => 0xff0000,
        'rosybrown' => 0xbc8f8f,
        'royalblue' => 0x4169e1,
        'saddlebrown' => 0x8b4513,
        'salmon' => 0xfa8072,
        'sandybrown' => 0xf4a460,
        'seagreen' => 0x2e8b57,
        'seashell' => 0xfff5ee,
        'sienna' => 0xa0522d,
        'silver' => 0xc0c0c0,
        'skyblue' => 0x87ceeb,
        'slateblue' => 0x6a5acd,
        'slategray' => 0x708090,
        'slategrey' => 0x708090,
        'snow' => 0xfffafa,
        'springgreen' => 0x00ff7f,
        'steelblue' => 0x4682b4,
        'tan' => 0xd2b48c,
        'teal' => 0x008080,
        'thistle' => 0xd8bfd8,
        'tomato' => 0xff6347,
        'turquoise' => 0x40e0d0,
        'violet' => 0xee82ee,
        'wheat' => 0xf5deb3,
        'white' => 0xffffff,
        'whitesmoke' => 0xf5f5f5,
        'yellow' => 0xffff00,
        'yellowgreen' => 0x9acd32
      }) {|color| (0..2).map {|n| color >> (n << 3) & 0xff}.reverse}

    # A hash from `[red, green, blue]` value arrays to color names.
    COLOR_NAMES_REVERSE = map_hash(hash_to_a(COLOR_NAMES)) {|k, v| [v, k]}

    # Constructs an RGB or HSL color object,
    # optionally with an alpha channel.
    # 
    # The RGB values must be between 0 and 255.
    # The saturation and lightness values must be between 0 and 100.
    # The alpha value must be between 0 and 1.
    #
    # @raise [Sass::SyntaxError] if any color value isn't in the specified range
    #
    # @overload initialize(attrs)
    #   The attributes are specified as a hash.
    #   This hash must contain either `:hue`, `:saturation`, and `:value` keys,
    #   or `:red`, `:green`, and `:blue` keys.
    #   It cannot contain both HSL and RGB keys.
    #   It may also optionally contain an `:alpha` key.
    #
    #   @param attrs [{Symbol => Numeric}] A hash of color attributes to values
    #   @raise [ArgumentError] if not enough attributes are specified,
    #     or both RGB and HSL attributes are specified
    #
    # @overload initialize(rgba)
    #   The attributes are specified as an array.
    #   This overload only supports RGB or RGBA colors.
    #
    #   @param rgba [Array<Numeric>] A three- or four-element array
    #     of the red, green, blue, and optionally alpha values (respectively)
    #     of the color
    #   @raise [ArgumentError] if not enough attributes are specified
    def initialize(attrs, allow_both_rgb_and_hsl = false)
      super(nil)

      if attrs.is_a?(Array)
        unless (3..4).include?(attrs.size)
          raise ArgumentError.new("Color.new(array) expects a three- or four-element array")
        end

        red, green, blue = attrs[0...3].map {|c| c.to_i}
        @attrs = {:red => red, :green => green, :blue => blue}
        @attrs[:alpha] = attrs[3] ? attrs[3].to_f : 1
      else
        attrs = attrs.reject {|k, v| v.nil?}
        hsl = [:hue, :saturation, :lightness] & attrs.keys
        rgb = [:red, :green, :blue] & attrs.keys
        if !allow_both_rgb_and_hsl && !hsl.empty? && !rgb.empty?
          raise ArgumentError.new("Color.new(hash) may not have both HSL and RGB keys specified")
        elsif hsl.empty? && rgb.empty?
          raise ArgumentError.new("Color.new(hash) must have either HSL or RGB keys specified")
        elsif !hsl.empty? && hsl.size != 3
          raise ArgumentError.new("Color.new(hash) must have all three HSL values specified")
        elsif !rgb.empty? && rgb.size != 3
          raise ArgumentError.new("Color.new(hash) must have all three RGB values specified")
        end

        @attrs = attrs
        @attrs[:hue] %= 360 if @attrs[:hue]
        @attrs[:alpha] ||= 1
      end

      [:red, :green, :blue].each do |k|
        next if @attrs[k].nil?
        @attrs[k] = @attrs[k].to_i
        Sass::Util.check_range("#{k.to_s.capitalize} value", 0..255, @attrs[k])
      end

      [:saturation, :lightness].each do |k|
        next if @attrs[k].nil?
        value = Number.new(@attrs[k], ['%']) # Get correct unit for error messages
        @attrs[k] = Sass::Util.check_range("#{k.to_s.capitalize}", 0..100, value, '%')
      end

      @attrs[:alpha] = Sass::Util.check_range("Alpha channel", 0..1, @attrs[:alpha])
    end

    # The red component of the color.
    #
    # @return [Fixnum]
    def red
      hsl_to_rgb!
      @attrs[:red]
    end

    # The green component of the color.
    #
    # @return [Fixnum]
    def green
      hsl_to_rgb!
      @attrs[:green]
    end

    # The blue component of the color.
    #
    # @return [Fixnum]
    def blue
      hsl_to_rgb!
      @attrs[:blue]
    end

    # The hue component of the color.
    #
    # @return [Numeric]
    def hue
      rgb_to_hsl!
      @attrs[:hue]
    end

    # The saturation component of the color.
    #
    # @return [Numeric]
    def saturation
      rgb_to_hsl!
      @attrs[:saturation]
    end

    # The lightness component of the color.
    #
    # @return [Numeric]
    def lightness
      rgb_to_hsl!
      @attrs[:lightness]
    end

    # The alpha channel (opacity) of the color.
    # This is 1 unless otherwise defined.
    #
    # @return [Fixnum]
    def alpha
      @attrs[:alpha]
    end

    # Returns whether this color object is translucent;
    # that is, whether the alpha channel is non-1.
    #
    # @return [Boolean]
    def alpha?
      alpha < 1
    end

    # Returns the red, green, and blue components of the color.
    #
    # @return [Array<Fixnum>] A frozen three-element array of the red, green, and blue
    #   values (respectively) of the color
    def rgb
      [red, green, blue].freeze
    end

    # Returns the hue, saturation, and lightness components of the color.
    #
    # @return [Array<Fixnum>] A frozen three-element array of the
    #   hue, saturation, and lightness values (respectively) of the color
    def hsl
      [hue, saturation, lightness].freeze
    end

    # The SassScript `==` operation.
    # **Note that this returns a {Sass::Script::Bool} object,
    # not a Ruby boolean**.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Bool] True if this literal is the same as the other,
    #   false otherwise
    def eq(other)
      Sass::Script::Bool.new(
        other.is_a?(Color) && rgb == other.rgb && alpha == other.alpha)
    end

    # Returns a copy of this color with one or more channels changed.
    # RGB or HSL colors may be changed, but not both at once.
    #
    # For example:
    #
    #     Color.new([10, 20, 30]).with(:blue => 40)
    #       #=> rgb(10, 40, 30)
    #     Color.new([126, 126, 126]).with(:red => 0, :green => 255)
    #       #=> rgb(0, 255, 126)
    #     Color.new([255, 0, 127]).with(:saturation => 60)
    #       #=> rgb(204, 51, 127)
    #     Color.new([1, 2, 3]).with(:alpha => 0.4)
    #       #=> rgba(1, 2, 3, 0.4)
    #
    # @param attrs [{Symbol => Numeric}]
    #   A map of channel names (`:red`, `:green`, `:blue`,
    #   `:hue`, `:saturation`, `:lightness`, or `:alpha`) to values
    # @return [Color] The new Color object
    # @raise [ArgumentError] if both RGB and HSL keys are specified
    def with(attrs)
      attrs = attrs.reject {|k, v| v.nil?}
      hsl = !([:hue, :saturation, :lightness] & attrs.keys).empty?
      rgb = !([:red, :green, :blue] & attrs.keys).empty?
      if hsl && rgb
        raise ArgumentError.new("Cannot specify HSL and RGB values for a color at the same time")
      end

      if hsl
        [:hue, :saturation, :lightness].each {|k| attrs[k] ||= send(k)}
      elsif rgb
        [:red, :green, :blue].each {|k| attrs[k] ||= send(k)}
      else
        # If we're just changing the alpha channel,
        # keep all the HSL/RGB stuff we've calculated
        attrs = @attrs.merge(attrs)
      end
      attrs[:alpha] ||= alpha

      Color.new(attrs, :allow_both_rgb_and_hsl)
    end

    # The SassScript `+` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Adds the number to each of the RGB color channels.
    #
    # {Color}
    # : Adds each of the RGB color channels together.
    #
    # {Literal}
    # : See {Literal#plus}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Color] The resulting color
    # @raise [Sass::SyntaxError] if `other` is a number with units
    def plus(other)
      if other.is_a?(Sass::Script::Number) || other.is_a?(Sass::Script::Color)
        piecewise(other, :+)
      else
        super
      end
    end

    # The SassScript `-` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Subtracts the number from each of the RGB color channels.
    #
    # {Color}
    # : Subtracts each of the other color's RGB color channels from this color's.
    #
    # {Literal}
    # : See {Literal#minus}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Color] The resulting color
    # @raise [Sass::SyntaxError] if `other` is a number with units
    def minus(other)
      if other.is_a?(Sass::Script::Number) || other.is_a?(Sass::Script::Color)
        piecewise(other, :-)
      else
        super
      end
    end

    # The SassScript `*` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Multiplies the number by each of the RGB color channels.
    #
    # {Color}
    # : Multiplies each of the RGB color channels together.
    #
    # @param other [Number, Color] The right-hand side of the operator
    # @return [Color] The resulting color
    # @raise [Sass::SyntaxError] if `other` is a number with units
    def times(other)
      if other.is_a?(Sass::Script::Number) || other.is_a?(Sass::Script::Color)
        piecewise(other, :*)
      else
        raise NoMethodError.new(nil, :times)
      end
    end

    # The SassScript `/` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Divides each of the RGB color channels by the number.
    #
    # {Color}
    # : Divides each of this color's RGB color channels by the other color's.
    #
    # {Literal}
    # : See {Literal#div}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Color] The resulting color
    # @raise [Sass::SyntaxError] if `other` is a number with units
    def div(other)
      if other.is_a?(Sass::Script::Number) || other.is_a?(Sass::Script::Color)
        piecewise(other, :/)
      else
        super
      end
    end

    # The SassScript `%` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Takes each of the RGB color channels module the number.
    #
    # {Color}
    # : Takes each of this color's RGB color channels modulo the other color's.
    #
    # @param other [Number, Color] The right-hand side of the operator
    # @return [Color] The resulting color
    # @raise [Sass::SyntaxError] if `other` is a number with units
    def mod(other)
      if other.is_a?(Sass::Script::Number) || other.is_a?(Sass::Script::Color)
        piecewise(other, :%)
      else
        raise NoMethodError.new(nil, :mod)
      end
    end

    # Returns a string representation of the color.
    # This is usually the color's hex value,
    # but if the color has a name that's used instead.
    #
    # @return [String] The string representation
    def to_s(opts = {})
      return rgba_str if alpha?
      return smallest if options[:style] == :compressed
      return COLOR_NAMES_REVERSE[rgb] if COLOR_NAMES_REVERSE[rgb]
      hex_str
    end
    alias_method :to_sass, :to_s

    # Returns a string representation of the color.
    #
    # @return [String] The hex value
    def inspect
      alpha? ? rgba_str : hex_str
    end

    private

    def smallest
      small_hex_str = hex_str.gsub(/^#(.)\1(.)\2(.)\3$/, '#\1\2\3')
      return small_hex_str unless (color = COLOR_NAMES_REVERSE[rgb]) &&
        color.size <= small_hex_str.size
      return color
    end

    def rgba_str
      split = options[:style] == :compressed ? ',' : ', '
      "rgba(#{rgb.join(split)}#{split}#{Number.round(alpha)})"
    end

    def hex_str
      red, green, blue = rgb.map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end

    def piecewise(other, operation)
      other_num = other.is_a? Number
      if other_num && !other.unitless?
        raise Sass::SyntaxError.new("Cannot add a number with units (#{other}) to a color (#{self}).") 
      end

      result = []
      for i in (0...3)
        res = rgb[i].send(operation, other_num ? other.value : other.rgb[i])
        result[i] = [ [res, 255].min, 0 ].max
      end

      if !other_num && other.alpha != alpha
        raise Sass::SyntaxError.new("Alpha channels must be equal: #{self} #{operation} #{other}")
      end

      with(:red => result[0], :green => result[1], :blue => result[2])
    end

    def hsl_to_rgb!
      return if @attrs[:red] && @attrs[:blue] && @attrs[:green]

      h = @attrs[:hue] / 360.0
      s = @attrs[:saturation] / 100.0
      l = @attrs[:lightness] / 100.0

      # Algorithm from the CSS3 spec: http://www.w3.org/TR/css3-color/#hsl-color.
      m2 = l <= 0.5 ? l * (s + 1) : l + s - l * s
      m1 = l * 2 - m2
      @attrs[:red], @attrs[:green], @attrs[:blue] = [
        hue_to_rgb(m1, m2, h + 1.0/3),
        hue_to_rgb(m1, m2, h),
        hue_to_rgb(m1, m2, h - 1.0/3)
      ].map {|c| (c * 0xff).round}
    end

    def hue_to_rgb(m1, m2, h)
      h += 1 if h < 0
      h -= 1 if h > 1
      return m1 + (m2 - m1) * h * 6 if h * 6 < 1
      return m2 if h * 2 < 1
      return m1 + (m2 - m1) * (2.0/3 - h) * 6 if h * 3 < 2
      return m1
    end

    def rgb_to_hsl!
      return if @attrs[:hue] && @attrs[:saturation] && @attrs[:lightness]
      r, g, b = [:red, :green, :blue].map {|k| @attrs[k] / 255.0}

      # Algorithm from http://en.wikipedia.org/wiki/HSL_and_HSV#Conversion_from_RGB_to_HSL_or_HSV
      max = [r, g, b].max
      min = [r, g, b].min
      d = max - min

      h =
        case max
        when min; 0
        when r; 60 * (g-b)/d
        when g; 60 * (b-r)/d + 120
        when b; 60 * (r-g)/d + 240
        end

      l = (max + min)/2.0

      s =
        if max == min
          0
        elsif l < 0.5
          d/(2*l)
        else
          d/(2 - 2*l)
        end

      @attrs[:hue] = h % 360
      @attrs[:saturation] = s * 100
      @attrs[:lightness] = l * 100
    end
  end
end
