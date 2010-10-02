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
  # \{#mix mix($color-1, $color-2\[, $weight\])}
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
  # \{#lightness lightness($color))}
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
  # ## List Functions
  #
  # \{#list list}
  # : Creates a space-delimited list
  #
  # \{#comma_list comma-list}
  # : Creates a comma-delimited list
  #
  # \{#nth nth}
  # : Returns the value at the nth index of a list
  #
  # \{#append append}
  # : Add one or more elements to the end of a list
  #
  # \{#prepend prepend}
  # : Add one or more elements to the beginning of a list
  #
  # \{#concat concat}
  # : Combine several lists into a single list
  #
  # \{#slice slice}
  # : Extract a sublist from a list
  #
  # \{#count count}
  # : Count how many elements are in a list
  #
  # \{#contains contains}
  # : Check whether a list contains all of the specified values
  #
  # \{#zip zip}
  # : Combine several lists of equal counts into one list
  #   where each element is a list of the values at that same
  #   index in the original lists.
  #
  # \{#map map}
  # : Create a new list where the elements are the results
  #   of successively calling the passed function with the
  #   list as the first argument.
  #
  # \{#apply apply}
  # : Calls a function using a list as the arguments
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
  #       define :reverse, :args => [:string]
  #     end
  #
  # Calling `define` after creating your function is how you tell
  # Sass what function signature(s) your function presents to the stylesheets.
  # If omitted, the function will still work, but will not be able to accept
  # named arguments. See {Sass::Script::Functions.define} for more information.
  #
  # There are a few things to keep in mind when modifying this module.
  # First of all, the arguments passed are {Sass::Script::Literal} objects.
  # Literal objects are also expected to be returned.
  # This means that Ruby values must be unwrapped and wrapped.
  #
  # Most Literal objects support the {Sass::Script::Literal#value value} accessor
  # for getting their Ruby values.
  # However, Color objects must be accessed using {Sass::Script::Color#rgb rgb},
  # {Sass::Script::Color#red red}, {Sass::Script::Color#blue green}, or {Sass::Script::Color#blue blue}
  # and List objects must be accessed via {Sass::Script::List#elements elements} or {Sass::Script::List#to_a to_a}.
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

require 'sass/script/functions/signatures'
require 'sass/script/functions/bool'
require 'sass/script/functions/color'
require 'sass/script/functions/list'
require 'sass/script/functions/introspection'
require 'sass/script/functions/number'
require 'sass/script/functions/string'
require 'sass/script/functions/evaluation_context'
