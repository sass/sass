require 'sass/constant/literal'

module Sass::Constant # :nodoc:
  class Color < Literal # :nodoc:

    HTML4_COLORS = {
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
    }

    REGEXP = /\##{"([0-9a-fA-F]{1,2})" * 3}/

    def parse(value)
      if (value =~ REGEXP)
        @value = value.scan(REGEXP)[0].map { |num| num.ljust(2, num).to_i(16) }
      else
        color = HTML4_COLORS[value.downcase]
        @value = (0..2).map{ |n| color >> (n << 3) & 0xff }.reverse
      end
    end

    def plus(other)
      if other.is_a? Sass::Constant::String
        Sass::Constant::String.from_value(self.to_s + other.to_s)
      else
        piecewise(other, :+)
      end
    end

    def minus(other)
      if other.is_a? Sass::Constant::String
        raise NoMethodError.new(nil, :minus)
      else
        piecewise(other, :-)
      end
    end

    def times(other)
      if other.is_a? Sass::Constant::String
        raise NoMethodError.new(nil, :times)
      else
        piecewise(other, :*)
      end
    end

    def div(other)
      if other.is_a? Sass::Constant::String
        raise NoMethodError.new(nil, :div)
      else
        piecewise(other, :/)
      end
    end

    def mod(other)
      if other.is_a? Sass::Constant::String
        raise NoMethodError.new(nil, :mod)
      else
        piecewise(other, :%)
      end
    end

    def to_s
      red, green, blue = @value.map { |num| num.to_s(16).rjust(2, '0') }
      "##{red}#{green}#{blue}"
    end

    protected

    def self.filter_value(value)
      value.map { |num| num.to_i }
    end

    private

    def piecewise(other, operation)
      other_num = other.is_a? Number
      other_val = other.value

      rgb = []
      for i in (0...3)
        res = @value[i].send(operation, other_num ? other_val : other_val[i])
        rgb[i] = [ [res, 255].min, 0 ].max
      end
      Color.from_value(rgb)
    end
  end
end
