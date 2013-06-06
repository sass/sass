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
  end
end
