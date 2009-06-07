require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a CSS color.
  class Color < Literal
    class << self; include Haml::Util; end

    # A hash from color names to [red, green, blue] value arrays.
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
    # A hash from [red, green, blue] value arrays to color names.
    HTML4_COLORS_REVERSE = map_hash(HTML4_COLORS) {|k, v| [v, k]}

    # @param rgb [Array<Fixnum>] A three-element array of the red, green, and blue values (respectively)
    #   of the color
    # @raise [Sass::SyntaxError] if any color value isn't between 0 and 255
    def initialize(rgb)
      rgb = rgb.map {|c| c.to_i}
      raise Sass::SyntaxError.new("Color values must be between 0 and 255") if rgb.any? {|c| c < 0 || c > 255}
      super(rgb)
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
    # {Literal}
    # : See {Literal#times}.
    #
    # @param other [Literal] The right-hand side of the operator
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
    # {Literal}
    # : See {Literal#mod}.
    #
    # @param other [Literal] The right-hand side of the operator
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
      return HTML4_COLORS_REVERSE[@value] if HTML4_COLORS_REVERSE[@value]
      red, green, blue = @value.map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end
    alias_method :inspect, :to_s

    private

    def piecewise(other, operation)
      other_num = other.is_a? Number
      other_val = other.value
      if other_num && !other.unitless?
        raise Sass::SyntaxError.new("Cannot add a number with units (#{other}) to a color (#{self}).") 
      end

      rgb = []
      for i in (0...3)
        res = @value[i].send(operation, other_num ? other_val : other_val[i])
        rgb[i] = [ [res, 255].min, 0 ].max
      end
      Color.new(rgb)
    end
  end
end
