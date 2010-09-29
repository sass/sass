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
  # \{#invert}
  # : Returns the inverse of a color.
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
    instance_methods.each { |m| undef_method m unless m.to_s =~ /^__/ }
  end
end

require 'sass/script/functions/color'
require 'sass/script/functions/list'
require 'sass/script/functions/meta'
require 'sass/script/functions/number'
require 'sass/script/functions/string'
require 'sass/script/functions/evaluation_context'
