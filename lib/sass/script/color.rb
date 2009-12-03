require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a CSS color.
  class Color < Literal
    class << self; include Haml::Util; end

    # A hash from color names to `[red, green, blue]` value arrays.
    HTML4_COLORS = map_vals({
        'black'   => 0x000000,
        'silver'  => 0xc0c0c0,
        'gray'    => 0x808080,
        'white'   => 0xffffff,
        'maroon'  => 0x800000,
        'red'     => 0xff0000,
        'purple'  => 0x800080,
        'fuchsia' => 0xff00ff,
        'green'   => 0x008000,
        'lime'    => 0x00ff00,
        'olive'   => 0x808000,
        'yellow'  => 0xffff00,
        'navy'    => 0x000080,
        'blue'    => 0x0000ff,
        'teal'    => 0x008080,
        'aqua'    => 0x00ffff
      }) {|color| (0..2).map {|n| color >> (n << 3) & 0xff}.reverse}
    # A hash from `[red, green, blue]` value arrays to color names.
    HTML4_COLORS_REVERSE = map_hash(HTML4_COLORS) {|k, v| [v, k]}

    # Constructs an RGB or RGBA color object.
    # The RGB values must be between 0 and 255,
    # and the alpha value is generally expected to be between 0 and 1.
    # However, the alpha value can be greater than 1
    # in order to allow it to be used for color multiplication.
    #
    # @param rgba [Array<Numeric>] A three-element array of the red, green, blue,
    #   and optionally alpha values (respectively) of the color
    # @raise [Sass::SyntaxError] if any color value isn't between 0 and 255,
    #   or the alpha value is negative
    def initialize(rgba)
      @red, @green, @blue = rgba[0...3].map {|c| c.to_i}
      @alpha = rgba[3] ? rgba[3].to_f : 1
      super(nil)

      unless rgb.all? {|c| (0..255).include?(c)}
        raise Sass::SyntaxError.new("Color values must be between 0 and 255")
      end

      unless (0..1).include?(alpha)
        raise Sass::SyntaxError.new("Color opacity value must between 0 and 1")
      end
    end

    # The red component of the color.
    #
    # @return [Fixnum]
    attr_reader :red

    # The green component of the color.
    #
    # @return [Fixnum]
    attr_reader :green

    # The blue component of the color.
    #
    # @return [Fixnum]
    attr_reader :blue

    # The alpha channel (opacity) of the color.
    # This is 1 unless otherwise defined.
    #
    # @return [Fixnum]
    attr_reader :alpha

    # Returns whether this color object is translucent;
    # that is, whether the alpha channel is non-1.
    #
    # @return [Boolean]
    def alpha?
      alpha < 1
    end

    # @deprecated This will be removed in version 2.6.
    # @see #rgb
    def value
      warn <<END
DEPRECATION WARNING:
The Sass::Script::Color #value attribute is deprecated and will be
removed in version 2.6. Use the #rgb attribute instead.
END
      rgb
    end

    # Returns the red, green, and blue components of the color.
    #
    # @return [Array<Fixnum>] A frozen three-element array of the red, green, and blue
    #   values (respectively) of the color
    def rgb
      [red, green, blue].freeze
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
    #
    # For example:
    #
    #     Color.new([10, 20, 30]).with(:blue => 40)
    #       #=> rgb(10, 40, 30)
    #     Color.new([126, 126, 126]).with(:red => 0, :green => 255)
    #       #=> rgb(0, 255, 126)
    #     Color.new([1, 2, 3]).with(:alpha => 0.4)
    #       #=> rgba(1, 2, 3, 0.4)
    #
    # @param attrs [{Symbol => Numeric}]
    #   A map of channel names (`:red`, `:green`, `:blue`, or `:alpha`) to values
    # @return [Color] The new Color object
    def with(attrs)
      Color.new([
          attrs[:red] || red,
          attrs[:green] || green,
          attrs[:blue] || blue,
          attrs[:alpha] || alpha,
        ])
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
    def to_s
      return rgba_str if alpha?
      return smallest if options[:style] == :compressed
      return HTML4_COLORS_REVERSE[rgb] if HTML4_COLORS_REVERSE[rgb]
      hex_str
    end
    alias_method :inspect, :to_s

    # Returns a string representation of the color.
    #
    # @return [String] The hex value
    def inspect
      alpha? ? rgba_str : hex_str
    end

    private

    def smallest
      small_hex_str = hex_str.gsub(/^#(.)\1(.)\2(.)\3$/, '#\1\2\3')
      return small_hex_str unless (color = HTML4_COLORS_REVERSE[rgb]) &&
        color.size <= small_hex_str.size
      return color
    end

    def rgba_str
      "rgba(#{rgb.join(', ')}, #{alpha % 1 == 0.0 ? alpha.to_i : alpha})"
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
  end
end
