#!/usr/bin/env ruby
require 'test/unit'
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/script'
require 'mock_importer'

module Sass::Script::Functions
  def no_kw_args
    Sass::Script::Value::String.new("no-kw-args")
  end

  def only_var_args(*args)
    Sass::Script::Value::String.new("only-var-args("+args.map{|a| a.plus(Sass::Script::Value::Number.new(1)).to_s }.join(", ")+")")
  end
  declare :only_var_args, [], :var_args => true

  def only_kw_args(kwargs)
    Sass::Script::Value::String.new("only-kw-args(" + kwargs.keys.map {|a| a.to_s}.sort.join(", ") + ")")
  end
  declare :only_kw_args, [], :var_kwargs => true
end

module Sass::Script::Functions::UserFunctions
  def call_options_on_new_value
    str = Sass::Script::Value::String.new("foo")
    str.options[:foo]
    str
  end

  def user_defined
    Sass::Script::Value::String.new("I'm a user-defined string!")
  end

  def _preceding_underscore
    Sass::Script::Value::String.new("I'm another user-defined string!")
  end

  def fetch_the_variable
    environment.var('variable')
  end
end

module Sass::Script::Functions
  include Sass::Script::Functions::UserFunctions
end

class SassFunctionTest < Test::Unit::TestCase
  # Tests taken from:
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-h-rotating-b.htm
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-values-b.htm
  File.read(File.dirname(__FILE__) + "/data/hsl-rgb.txt").split("\n\n").each do |chunk|
    hsls, rgbs = chunk.strip.split("====")
    hsls.strip.split("\n").zip(rgbs.strip.split("\n")) do |hsl, rgb|
      hsl_method = "test_hsl: #{hsl} = #{rgb}"
      unless method_defined?(hsl_method)
        define_method(hsl_method) do
          assert_equal(evaluate(rgb), evaluate(hsl))
        end
      end

      rgb_to_hsl_method = "test_rgb_to_hsl: #{rgb} = #{hsl}"
      unless method_defined?(rgb_to_hsl_method)
        define_method(rgb_to_hsl_method) do
          rgb_color = perform(rgb)
          hsl_color = perform(hsl)

          white = hsl_color.lightness == 100
          black = hsl_color.lightness == 0
          grayscale = white || black || hsl_color.saturation == 0

          assert_in_delta(hsl_color.hue, rgb_color.hue, 0.0001,
            "Hues should be equal") unless grayscale
          assert_in_delta(hsl_color.saturation, rgb_color.saturation, 0.0001,
            "Saturations should be equal") unless white || black
          assert_in_delta(hsl_color.lightness, rgb_color.lightness, 0.0001,
            "Lightnesses should be equal")
        end
      end
    end
  end

  def test_hsl_kwargs
    assert_equal "#33cccc", evaluate("hsl($hue: 180, $saturation: 60%, $lightness: 50%)")
  end

  def test_hsl_checks_bounds
    assert_error_message("Saturation -114 must be between 0% and 100% for `hsl'", "hsl(10, -114, 12)");
    assert_error_message("Lightness 256% must be between 0% and 100% for `hsl'", "hsl(10, 10, 256%)");
  end

  def test_hsl_checks_types
    assert_error_message("$hue: \"foo\" is not a number for `hsl'", "hsl(\"foo\", 10, 12)");
    assert_error_message("$saturation: \"foo\" is not a number for `hsl'", "hsl(10, \"foo\", 12)");
    assert_error_message("$lightness: \"foo\" is not a number for `hsl'", "hsl(10, 10, \"foo\")");
  end

  def test_hsla
    assert_equal "rgba(51, 204, 204, 0.4)", evaluate("hsla(180, 60%, 50%, 0.4)")
    assert_equal "#33cccc", evaluate("hsla(180, 60%, 50%, 1)")
    assert_equal "rgba(51, 204, 204, 0)", evaluate("hsla(180, 60%, 50%, 0)")
    assert_equal "rgba(51, 204, 204, 0.4)", evaluate("hsla($hue: 180, $saturation: 60%, $lightness: 50%, $alpha: 0.4)")
  end

  def test_hsla_checks_bounds
    assert_error_message("Saturation -114 must be between 0% and 100% for `hsla'", "hsla(10, -114, 12, 1)");
    assert_error_message("Lightness 256% must be between 0% and 100% for `hsla'", "hsla(10, 10, 256%, 0)");
    assert_error_message("Alpha channel -0.1 must be between 0 and 1 for `hsla'", "hsla(10, 10, 10, -0.1)");
    assert_error_message("Alpha channel 1.1 must be between 0 and 1 for `hsla'", "hsla(10, 10, 10, 1.1)");
  end

  def test_hsla_checks_types
    assert_error_message("$hue: \"foo\" is not a number for `hsla'", "hsla(\"foo\", 10, 12, 0.3)");
    assert_error_message("$saturation: \"foo\" is not a number for `hsla'", "hsla(10, \"foo\", 12, 0)");
    assert_error_message("$lightness: \"foo\" is not a number for `hsla'", "hsla(10, 10, \"foo\", 1)");
    assert_error_message("$alpha: \"foo\" is not a number for `hsla'", "hsla(10, 10, 10, \"foo\")");
  end

  def test_percentage
    assert_equal("50%",  evaluate("percentage(.5)"))
    assert_equal("100%", evaluate("percentage(1)"))
    assert_equal("25%",  evaluate("percentage(25px / 100px)"))
    assert_equal("50%",  evaluate("percentage($number: 0.5)"))
  end

  def test_percentage_deprecated_arg_name
    assert_warning(<<WARNING) {assert_equal("50%", evaluate("percentage($value: 0.5)"))}
DEPRECATION WARNING: The `$value' argument for `percentage()' has been renamed to `$number'.
WARNING
  end

  def test_percentage_checks_types
    assert_error_message("$number: 25px is not a unitless number for `percentage'", "percentage(25px)")
    assert_error_message("$number: #cccccc is not a unitless number for `percentage'", "percentage(#ccc)")
    assert_error_message("$number: \"string\" is not a unitless number for `percentage'", %Q{percentage("string")})
  end

  def test_round
    assert_equal("5",   evaluate("round(4.8)"))
    assert_equal("5px", evaluate("round(4.8px)"))
    assert_equal("5px", evaluate("round(5.49px)"))
    assert_equal("5px", evaluate("round($number: 5.49px)"))
  end

  def test_round_deprecated_arg_name
    assert_warning(<<WARNING) {assert_equal("5px", evaluate("round($value: 5.49px)"))}
DEPRECATION WARNING: The `$value' argument for `round()' has been renamed to `$number'.
WARNING
  end

  def test_round_checks_types
    assert_error_message("$value: #cccccc is not a number for `round'", "round(#ccc)")
  end

  def test_floor
    assert_equal("4",   evaluate("floor(4.8)"))
    assert_equal("4px", evaluate("floor(4.8px)"))
    assert_equal("4px", evaluate("floor($number: 4.8px)"))
  end

  def test_floor_deprecated_arg_name
    assert_warning(<<WARNING) {assert_equal("4px", evaluate("floor($value: 4.8px)"))}
DEPRECATION WARNING: The `$value' argument for `floor()' has been renamed to `$number'.
WARNING
  end

  def test_floor_checks_types
    assert_error_message("$value: \"foo\" is not a number for `floor'", "floor(\"foo\")")
  end

  def test_ceil
    assert_equal("5",   evaluate("ceil(4.1)"))
    assert_equal("5px", evaluate("ceil(4.8px)"))
    assert_equal("5px", evaluate("ceil($number: 4.8px)"))
  end

  def test_ceil_deprecated_arg_name
    assert_warning(<<WARNING) {assert_equal("5px", evaluate("ceil($value: 4.8px)"))}
DEPRECATION WARNING: The `$value' argument for `ceil()' has been renamed to `$number'.
WARNING
  end

  def test_ceil_checks_types
    assert_error_message("$value: \"a\" is not a number for `ceil'", "ceil(\"a\")")
  end

  def test_abs
    assert_equal("5",   evaluate("abs(-5)"))
    assert_equal("5px", evaluate("abs(-5px)"))
    assert_equal("5",   evaluate("abs(5)"))
    assert_equal("5px", evaluate("abs(5px)"))
    assert_equal("5px", evaluate("abs($number: 5px)"))
  end

  def test_abs_deprecated_arg_name
    assert_warning(<<WARNING) {assert_equal("5px", evaluate("abs($value: 5px)"))}
DEPRECATION WARNING: The `$value' argument for `abs()' has been renamed to `$number'.
WARNING
  end

  def test_abs_checks_types
    assert_error_message("$value: #aaaaaa is not a number for `abs'", "abs(#aaa)")
  end

  def test_min
    assert_equal("1", evaluate("min(1, 2, 3)"))
    assert_equal("1", evaluate("min(3px, 2px, 1)"))
    assert_equal("4em", evaluate("min(4em)"))
    assert_equal("10cm", evaluate("min(10cm, 6in)"))

    assert_error_message("#aaaaaa is not a number for `min'", "min(#aaa)")
    assert_error_message("Incompatible units: 'px' and 'em'.", "min(3em, 4em, 1px)")
  end

  def test_max
    assert_equal("3", evaluate("max(1, 2, 3)"))
    assert_equal("3", evaluate("max(3, 2px, 1px)"))
    assert_equal("4em", evaluate("max(4em)"))
    assert_equal("6in", evaluate("max(10cm, 6in)"))

    assert_error_message("#aaaaaa is not a number for `max'", "max(#aaa)")
    assert_error_message("Incompatible units: 'px' and 'em'.", "max(3em, 4em, 1px)")
  end

  def test_rgb
    assert_equal("#123456", evaluate("rgb(18, 52, 86)"))
    assert_equal("#beaded", evaluate("rgb(190, 173, 237)"))
    assert_equal("springgreen", evaluate("rgb(0, 255, 127)"))
    assert_equal("springgreen", evaluate("rgb($red: 0, $green: 255, $blue: 127)"))
  end

  def test_rgb_percent
    assert_equal("#123456", evaluate("rgb(7.1%, 20.4%, 34%)"))
    assert_equal("#beaded", evaluate("rgb(74.7%, 173, 93%)"))
    assert_equal("#beaded", evaluate("rgb(190, 68%, 237)"))
    assert_equal("springgreen", evaluate("rgb(0%, 100%, 50%)"))
  end

  def test_rgb_tests_bounds
    assert_error_message("$red: Color value 256 must be between 0 and 255 for `rgb'",
      "rgb(256, 1, 1)")
    assert_error_message("$green: Color value 256 must be between 0 and 255 for `rgb'",
      "rgb(1, 256, 1)")
    assert_error_message("$blue: Color value 256 must be between 0 and 255 for `rgb'",
      "rgb(1, 1, 256)")
    assert_error_message("$green: Color value 256 must be between 0 and 255 for `rgb'",
      "rgb(1, 256, 257)")
    assert_error_message("$red: Color value -1 must be between 0 and 255 for `rgb'",
      "rgb(-1, 1, 1)")
  end

  def test_rgb_test_percent_bounds
    assert_error_message("$red: Color value 100.1% must be between 0% and 100% for `rgb'",
      "rgb(100.1%, 0, 0)")
    assert_error_message("$green: Color value -0.1% must be between 0% and 100% for `rgb'",
      "rgb(0, -0.1%, 0)")
    assert_error_message("$blue: Color value 101% must be between 0% and 100% for `rgb'",
      "rgb(0, 0, 101%)")
  end

  def test_rgb_tests_types
    assert_error_message("$red: \"foo\" is not a number for `rgb'", "rgb(\"foo\", 10, 12)");
    assert_error_message("$green: \"foo\" is not a number for `rgb'", "rgb(10, \"foo\", 12)");
    assert_error_message("$blue: \"foo\" is not a number for `rgb'", "rgb(10, 10, \"foo\")");
  end

  def test_rgba
    assert_equal("rgba(18, 52, 86, 0.5)", evaluate("rgba(18, 52, 86, 0.5)"))
    assert_equal("#beaded", evaluate("rgba(190, 173, 237, 1)"))
    assert_equal("rgba(0, 255, 127, 0)", evaluate("rgba(0, 255, 127, 0)"))
    assert_equal("rgba(0, 255, 127, 0)", evaluate("rgba($red: 0, $green: 255, $blue: 127, $alpha: 0)"))
  end

  def test_rgba_tests_bounds
    assert_error_message("$red: Color value 256 must be between 0 and 255 for `rgba'",
      "rgba(256, 1, 1, 0.3)")
    assert_error_message("$green: Color value 256 must be between 0 and 255 for `rgba'",
      "rgba(1, 256, 1, 0.3)")
    assert_error_message("$blue: Color value 256 must be between 0 and 255 for `rgba'",
      "rgba(1, 1, 256, 0.3)")
    assert_error_message("$green: Color value 256 must be between 0 and 255 for `rgba'",
      "rgba(1, 256, 257, 0.3)")
    assert_error_message("$red: Color value -1 must be between 0 and 255 for `rgba'",
      "rgba(-1, 1, 1, 0.3)")
    assert_error_message("Alpha channel -0.2 must be between 0 and 1 for `rgba'",
      "rgba(1, 1, 1, -0.2)")
    assert_error_message("Alpha channel 1.2 must be between 0 and 1 for `rgba'",
      "rgba(1, 1, 1, 1.2)")
  end

  def test_rgba_tests_types
    assert_error_message("$red: \"foo\" is not a number for `rgba'", "rgba(\"foo\", 10, 12, 0.2)");
    assert_error_message("$green: \"foo\" is not a number for `rgba'", "rgba(10, \"foo\", 12, 0.1)");
    assert_error_message("$blue: \"foo\" is not a number for `rgba'", "rgba(10, 10, \"foo\", 0)");
    assert_error_message("$alpha: \"foo\" is not a number for `rgba'", "rgba(10, 10, 10, \"foo\")");
  end

  def test_rgba_with_color
    assert_equal "rgba(16, 32, 48, 0.5)", evaluate("rgba(#102030, 0.5)")
    assert_equal "rgba(0, 0, 255, 0.5)", evaluate("rgba(blue, 0.5)")
    assert_equal "rgba(0, 0, 255, 0.5)", evaluate("rgba($color: blue, $alpha: 0.5)")
  end

  def test_rgba_with_color_tests_types
    assert_error_message("$color: \"foo\" is not a color for `rgba'", "rgba(\"foo\", 0.2)");
    assert_error_message("$alpha: \"foo\" is not a number for `rgba'", "rgba(blue, \"foo\")");
  end

  def test_rgba_tests_num_args
    assert_error_message("wrong number of arguments (0 for 4) for `rgba'", "rgba()");
    assert_error_message("wrong number of arguments (1 for 4) for `rgba'", "rgba(blue)");
    assert_error_message("wrong number of arguments (3 for 4) for `rgba'", "rgba(1, 2, 3)");
    assert_error_message("wrong number of arguments (5 for 4) for `rgba'", "rgba(1, 2, 3, 0.4, 5)");
  end

  def test_red
    assert_equal("18", evaluate("red(#123456)"))
    assert_equal("18", evaluate("red($color: #123456)"))
  end

  def test_red_exception
    assert_error_message("$color: 12 is not a color for `red'", "red(12)")
  end

  def test_green
    assert_equal("52", evaluate("green(#123456)"))
    assert_equal("52", evaluate("green($color: #123456)"))
  end

  def test_green_exception
    assert_error_message("$color: 12 is not a color for `green'", "green(12)")
  end

  def test_blue
    assert_equal("86", evaluate("blue(#123456)"))
    assert_equal("86", evaluate("blue($color: #123456)"))
  end

  def test_blue_exception
    assert_error_message("$color: 12 is not a color for `blue'", "blue(12)")
  end

  def test_hue
    assert_equal("18deg", evaluate("hue(hsl(18, 50%, 20%))"))
    assert_equal("18deg", evaluate("hue($color: hsl(18, 50%, 20%))"))
  end

  def test_hue_exception
    assert_error_message("$color: 12 is not a color for `hue'", "hue(12)")
  end

  def test_saturation
    assert_equal("52%", evaluate("saturation(hsl(20, 52%, 20%))"))
    assert_equal("52%", evaluate("saturation(hsl(20, 52, 20%))"))
    assert_equal("52%", evaluate("saturation($color: hsl(20, 52, 20%))"))
  end

  def test_saturation_exception
    assert_error_message("$color: 12 is not a color for `saturation'", "saturation(12)")
  end

  def test_lightness
    assert_equal("86%", evaluate("lightness(hsl(120, 50%, 86%))"))
    assert_equal("86%", evaluate("lightness(hsl(120, 50%, 86))"))
    assert_equal("86%", evaluate("lightness($color: hsl(120, 50%, 86))"))
  end

  def test_lightness_exception
    assert_error_message("$color: 12 is not a color for `lightness'", "lightness(12)")
  end

  def test_alpha
    assert_equal("1", evaluate("alpha(#123456)"))
    assert_equal("0.34", evaluate("alpha(rgba(0, 1, 2, 0.34))"))
    assert_equal("0", evaluate("alpha(hsla(0, 1, 2, 0))"))
    assert_equal("0", evaluate("alpha($color: hsla(0, 1, 2, 0))"))
  end

  def test_alpha_exception
    assert_error_message("$color: 12 is not a color for `alpha'", "alpha(12)")
  end

  def test_opacity
    assert_equal("1", evaluate("opacity(#123456)"))
    assert_equal("0.34", evaluate("opacity(rgba(0, 1, 2, 0.34))"))
    assert_equal("0", evaluate("opacity(hsla(0, 1, 2, 0))"))
    assert_equal("0", evaluate("opacity($color: hsla(0, 1, 2, 0))"))
    assert_equal("opacity(20%)", evaluate("opacity(20%)"))
  end

  def test_opacity_exception
    assert_error_message("$color: \"foo\" is not a color for `opacity'", "opacity(foo)")
  end

  def test_opacify
    assert_equal("rgba(0, 0, 0, 0.75)", evaluate("opacify(rgba(0, 0, 0, 0.5), 0.25)"))
    assert_equal("rgba(0, 0, 0, 0.3)", evaluate("opacify(rgba(0, 0, 0, 0.2), 0.1)"))
    assert_equal("rgba(0, 0, 0, 0.7)", evaluate("fade-in(rgba(0, 0, 0, 0.2), 0.5px)"))
    assert_equal("black", evaluate("fade_in(rgba(0, 0, 0, 0.2), 0.8)"))
    assert_equal("black", evaluate("opacify(rgba(0, 0, 0, 0.2), 1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("opacify(rgba(0, 0, 0, 0.2), 0%)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("opacify($color: rgba(0, 0, 0, 0.2), $amount: 0%)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("fade-in($color: rgba(0, 0, 0, 0.2), $amount: 0%)"))
  end

  def test_opacify_tests_bounds
    assert_error_message("Amount -0.001 must be between 0 and 1 for `opacify'",
      "opacify(rgba(0, 0, 0, 0.2), -0.001)")
    assert_error_message("Amount 1.001 must be between 0 and 1 for `opacify'",
      "opacify(rgba(0, 0, 0, 0.2), 1.001)")
  end

  def test_opacify_tests_types
    assert_error_message("$color: \"foo\" is not a color for `opacify'", "opacify(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `opacify'", "opacify(#fff, \"foo\")")
  end

  def test_transparentize
    assert_equal("rgba(0, 0, 0, 0.3)", evaluate("transparentize(rgba(0, 0, 0, 0.5), 0.2)"))
    assert_equal("rgba(0, 0, 0, 0.1)", evaluate("transparentize(rgba(0, 0, 0, 0.2), 0.1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("fade-out(rgba(0, 0, 0, 0.5), 0.3px)"))
    assert_equal("transparent", evaluate("fade_out(rgba(0, 0, 0, 0.2), 0.2)"))
    assert_equal("transparent", evaluate("transparentize(rgba(0, 0, 0, 0.2), 1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("transparentize(rgba(0, 0, 0, 0.2), 0)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("transparentize($color: rgba(0, 0, 0, 0.2), $amount: 0)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("fade-out($color: rgba(0, 0, 0, 0.2), $amount: 0)"))
  end

  def test_transparentize_tests_bounds
    assert_error_message("Amount -0.001 must be between 0 and 1 for `transparentize'",
      "transparentize(rgba(0, 0, 0, 0.2), -0.001)")
    assert_error_message("Amount 1.001 must be between 0 and 1 for `transparentize'",
      "transparentize(rgba(0, 0, 0, 0.2), 1.001)")
  end

  def test_transparentize_tests_types
    assert_error_message("$color: \"foo\" is not a color for `transparentize'", "transparentize(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `transparentize'", "transparentize(#fff, \"foo\")")
  end

  def test_lighten
    assert_equal("#4d4d4d", evaluate("lighten(hsl(0, 0, 0), 30%)"))
    assert_equal("#ee0000", evaluate("lighten(#800, 20%)"))
    assert_equal("white", evaluate("lighten(#fff, 20%)"))
    assert_equal("white", evaluate("lighten(#800, 100%)"))
    assert_equal("#880000", evaluate("lighten(#800, 0%)"))
    assert_equal("rgba(238, 0, 0, 0.5)", evaluate("lighten(rgba(136, 0, 0, 0.5), 20%)"))
    assert_equal("rgba(238, 0, 0, 0.5)", evaluate("lighten($color: rgba(136, 0, 0, 0.5), $amount: 20%)"))
  end

  def test_lighten_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `lighten'",
      "lighten(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `lighten'",
      "lighten(#123, 100.001)")
  end

  def test_lighten_tests_types
    assert_error_message("$color: \"foo\" is not a color for `lighten'", "lighten(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `lighten'", "lighten(#fff, \"foo\")")
  end

  def test_darken
    assert_equal("#ff6a00", evaluate("darken(hsl(25, 100, 80), 30%)"))
    assert_equal("#220000", evaluate("darken(#800, 20%)"))
    assert_equal("black", evaluate("darken(#000, 20%)"))
    assert_equal("black", evaluate("darken(#800, 100%)"))
    assert_equal("#880000", evaluate("darken(#800, 0%)"))
    assert_equal("rgba(34, 0, 0, 0.5)", evaluate("darken(rgba(136, 0, 0, 0.5), 20%)"))
    assert_equal("rgba(34, 0, 0, 0.5)", evaluate("darken($color: rgba(136, 0, 0, 0.5), $amount: 20%)"))
  end

  def test_darken_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `darken'",
      "darken(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `darken'",
      "darken(#123, 100.001)")
  end

  def test_darken_tests_types
    assert_error_message("$color: \"foo\" is not a color for `darken'", "darken(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `darken'", "darken(#fff, \"foo\")")
  end

  def test_saturate
    assert_equal("#d9f2d9", evaluate("saturate(hsl(120, 30, 90), 20%)"))
    assert_equal("#9e3f3f", evaluate("saturate(#855, 20%)"))
    assert_equal("black", evaluate("saturate(#000, 20%)"))
    assert_equal("white", evaluate("saturate(#fff, 20%)"))
    assert_equal("#33ff33", evaluate("saturate(#8a8, 100%)"))
    assert_equal("#88aa88", evaluate("saturate(#8a8, 0%)"))
    assert_equal("rgba(158, 63, 63, 0.5)", evaluate("saturate(rgba(136, 85, 85, 0.5), 20%)"))
    assert_equal("rgba(158, 63, 63, 0.5)", evaluate("saturate($color: rgba(136, 85, 85, 0.5), $amount: 20%)"))
    assert_equal("saturate(50%)", evaluate("saturate(50%)"))
  end

  def test_saturate_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `saturate'",
      "saturate(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `saturate'",
      "saturate(#123, 100.001)")
  end

  def test_saturate_tests_types
    assert_error_message("$color: \"foo\" is not a color for `saturate'", "saturate(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `saturate'", "saturate(#fff, \"foo\")")
  end

  def test_desaturate
    assert_equal("#e3e8e3", evaluate("desaturate(hsl(120, 30, 90), 20%)"))
    assert_equal("#726b6b", evaluate("desaturate(#855, 20%)"))
    assert_equal("black", evaluate("desaturate(#000, 20%)"))
    assert_equal("white", evaluate("desaturate(#fff, 20%)"))
    assert_equal("#999999", evaluate("desaturate(#8a8, 100%)"))
    assert_equal("#88aa88", evaluate("desaturate(#8a8, 0%)"))
    assert_equal("rgba(114, 107, 107, 0.5)", evaluate("desaturate(rgba(136, 85, 85, 0.5), 20%)"))
    assert_equal("rgba(114, 107, 107, 0.5)", evaluate("desaturate($color: rgba(136, 85, 85, 0.5), $amount: 20%)"))
  end

  def test_desaturate_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `desaturate'",
      "desaturate(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `desaturate'",
      "desaturate(#123, 100.001)")
  end

  def test_desaturate_tests_types
    assert_error_message("$color: \"foo\" is not a color for `desaturate'", "desaturate(\"foo\", 10%)")
    assert_error_message("$amount: \"foo\" is not a number for `desaturate'", "desaturate(#fff, \"foo\")")
  end

  def test_adjust_hue
    assert_equal("#deeded", evaluate("adjust-hue(hsl(120, 30, 90), 60deg)"))
    assert_equal("#ededde", evaluate("adjust-hue(hsl(120, 30, 90), -60deg)"))
    assert_equal("#886a11", evaluate("adjust-hue(#811, 45deg)"))
    assert_equal("black", evaluate("adjust-hue(#000, 45deg)"))
    assert_equal("white", evaluate("adjust-hue(#fff, 45deg)"))
    assert_equal("#88aa88", evaluate("adjust-hue(#8a8, 360deg)"))
    assert_equal("#88aa88", evaluate("adjust-hue(#8a8, 0deg)"))
    assert_equal("rgba(136, 106, 17, 0.5)", evaluate("adjust-hue(rgba(136, 17, 17, 0.5), 45deg)"))
    assert_equal("rgba(136, 106, 17, 0.5)", evaluate("adjust-hue($color: rgba(136, 17, 17, 0.5), $degrees: 45deg)"))
  end

  def test_adjust_hue_tests_types
    assert_error_message("$color: \"foo\" is not a color for `adjust-hue'", "adjust-hue(\"foo\", 10%)")
    assert_error_message("$degrees: \"foo\" is not a number for `adjust-hue'", "adjust-hue(#fff, \"foo\")")
  end

  def test_adjust_color
    # HSL
    assert_equal(evaluate("hsl(180, 30, 90)"),
      evaluate("adjust-color(hsl(120, 30, 90), $hue: 60deg)"))
    assert_equal(evaluate("hsl(120, 50, 90)"),
      evaluate("adjust-color(hsl(120, 30, 90), $saturation: 20%)"))
    assert_equal(evaluate("hsl(120, 30, 60)"),
      evaluate("adjust-color(hsl(120, 30, 90), $lightness: -30%)"))
    # RGB
    assert_equal(evaluate("rgb(15, 20, 30)"),
      evaluate("adjust-color(rgb(10, 20, 30), $red: 5)"))
    assert_equal(evaluate("rgb(10, 15, 30)"),
      evaluate("adjust-color(rgb(10, 20, 30), $green: -5)"))
    assert_equal(evaluate("rgb(10, 20, 40)"),
      evaluate("adjust-color(rgb(10, 20, 30), $blue: 10)"))
    # Alpha
    assert_equal(evaluate("hsla(120, 30, 90, 0.65)"),
      evaluate("adjust-color(hsl(120, 30, 90), $alpha: -0.35)"))
    assert_equal(evaluate("rgba(10, 20, 30, 0.9)"),
      evaluate("adjust-color(rgba(10, 20, 30, 0.4), $alpha: 0.5)"))

    # HSL composability
    assert_equal(evaluate("hsl(180, 20, 90)"),
      evaluate("adjust-color(hsl(120, 30, 90), $hue: 60deg, $saturation: -10%)"))
    assert_equal(evaluate("hsl(180, 20, 95)"),
      evaluate("adjust-color(hsl(120, 30, 90), $hue: 60deg, $saturation: -10%, $lightness: 5%)"))
    assert_equal(evaluate("hsla(120, 20, 95, 0.3)"),
      evaluate("adjust-color(hsl(120, 30, 90), $saturation: -10%, $lightness: 5%, $alpha: -0.7)"))

    # RGB composability
    assert_equal(evaluate("rgb(15, 20, 29)"),
      evaluate("adjust-color(rgb(10, 20, 30), $red: 5, $blue: -1)"))
    assert_equal(evaluate("rgb(15, 45, 29)"),
      evaluate("adjust-color(rgb(10, 20, 30), $red: 5, $green: 25, $blue: -1)"))
    assert_equal(evaluate("rgba(10, 25, 29, 0.7)"),
      evaluate("adjust-color(rgb(10, 20, 30), $green: 5, $blue: -1, $alpha: -0.3)"))

    # HSL range restriction
    assert_equal(evaluate("hsl(120, 30, 90)"),
      evaluate("adjust-color(hsl(120, 30, 90), $hue: 720deg)"))
    assert_equal(evaluate("hsl(120, 0, 90)"),
      evaluate("adjust-color(hsl(120, 30, 90), $saturation: -90%)"))
    assert_equal(evaluate("hsl(120, 30, 100)"),
      evaluate("adjust-color(hsl(120, 30, 90), $lightness: 30%)"))

    # RGB range restriction
    assert_equal(evaluate("rgb(255, 20, 30)"),
      evaluate("adjust-color(rgb(10, 20, 30), $red: 250)"))
    assert_equal(evaluate("rgb(10, 0, 30)"),
      evaluate("adjust-color(rgb(10, 20, 30), $green: -30)"))
    assert_equal(evaluate("rgb(10, 20, 0)"),
      evaluate("adjust-color(rgb(10, 20, 30), $blue: -40)"))
  end

  def test_adjust_color_tests_types
    assert_error_message("$color: \"foo\" is not a color for `adjust-color'", "adjust-color(foo, $hue: 10)")
    # HSL
    assert_error_message("$hue: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $hue: foo)")
    assert_error_message("$saturation: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $saturation: foo)")
    assert_error_message("$lightness: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $lightness: foo)")
    # RGB
    assert_error_message("$red: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $red: foo)")
    assert_error_message("$green: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $green: foo)")
    assert_error_message("$blue: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $blue: foo)")
    # Alpha
    assert_error_message("$alpha: \"foo\" is not a number for `adjust-color'",
      "adjust-color(blue, $alpha: foo)")
  end

  def test_adjust_color_tests_arg_range
    # HSL
    assert_error_message("$saturation: Amount 101% must be between -100% and 100% for `adjust-color'",
      "adjust-color(blue, $saturation: 101%)")
    assert_error_message("$saturation: Amount -101% must be between -100% and 100% for `adjust-color'",
      "adjust-color(blue, $saturation: -101%)")
    assert_error_message("$lightness: Amount 101% must be between -100% and 100% for `adjust-color'",
      "adjust-color(blue, $lightness: 101%)")
    assert_error_message("$lightness: Amount -101% must be between -100% and 100% for `adjust-color'",
      "adjust-color(blue, $lightness: -101%)")
    # RGB
    assert_error_message("$red: Amount 256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $red: 256)")
    assert_error_message("$red: Amount -256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $red: -256)")
    assert_error_message("$green: Amount 256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $green: 256)")
    assert_error_message("$green: Amount -256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $green: -256)")
    assert_error_message("$blue: Amount 256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $blue: 256)")
    assert_error_message("$blue: Amount -256 must be between -255 and 255 for `adjust-color'",
      "adjust-color(blue, $blue: -256)")
    # Alpha
    assert_error_message("$alpha: Amount 1.1 must be between -1 and 1 for `adjust-color'",
      "adjust-color(blue, $alpha: 1.1)")
    assert_error_message("$alpha: Amount -1.1 must be between -1 and 1 for `adjust-color'",
      "adjust-color(blue, $alpha: -1.1)")
  end

  def test_adjust_color_argument_errors
    assert_error_message("Unknown argument $hoo (260deg) for `adjust-color'",
      "adjust-color(blue, $hoo: 260deg)")
    assert_error_message("Cannot specify HSL and RGB values for a color at the same time for `adjust-color'",
      "adjust-color(blue, $hue: 120deg, $red: 10)");
    assert_error_message("10px is not a keyword argument for `adjust_color'",
      "adjust-color(blue, 10px)")
    assert_error_message("10px is not a keyword argument for `adjust_color'",
      "adjust-color(blue, 10px, 20px)")
    assert_error_message("10px is not a keyword argument for `adjust_color'",
      "adjust-color(blue, 10px, $hue: 180deg)")
  end

  def test_scale_color
    # HSL
    assert_equal(evaluate("hsl(120, 51, 90)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: 30%)"))
    assert_equal(evaluate("hsl(120, 30, 76.5)"),
      evaluate("scale-color(hsl(120, 30, 90), $lightness: -15%)"))
    # RGB
    assert_equal(evaluate("rgb(157, 20, 30)"),
      evaluate("scale-color(rgb(10, 20, 30), $red: 60%)"))
    assert_equal(evaluate("rgb(10, 38.8, 30)"),
      evaluate("scale-color(rgb(10, 20, 30), $green: 8%)"))
    assert_equal(evaluate("rgb(10, 20, 20)"),
      evaluate("scale-color(rgb(10, 20, 30), $blue: -(1/3)*100%)"))
    # Alpha
    assert_equal(evaluate("hsla(120, 30, 90, 0.86)"),
      evaluate("scale-color(hsl(120, 30, 90), $alpha: -14%)"))
    assert_equal(evaluate("rgba(10, 20, 30, 0.82)"),
      evaluate("scale-color(rgba(10, 20, 30, 0.8), $alpha: 10%)"))

    # HSL composability
    assert_equal(evaluate("hsl(120, 51, 76.5)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: 30%, $lightness: -15%)"))
    assert_equal(evaluate("hsla(120, 51, 90, 0.2)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: 30%, $alpha: -80%)"))

    # RGB composability
    assert_equal(evaluate("rgb(157, 38.8, 30)"),
      evaluate("scale-color(rgb(10, 20, 30), $red: 60%, $green: 8%)"))
    assert_equal(evaluate("rgb(157, 38.8, 20)"),
      evaluate("scale-color(rgb(10, 20, 30), $red: 60%, $green: 8%, $blue: -(1/3)*100%)"))
    assert_equal(evaluate("rgba(10, 38.8, 20, 0.55)"),
      evaluate("scale-color(rgba(10, 20, 30, 0.5), $green: 8%, $blue: -(1/3)*100%, $alpha: 10%)"))

    # Extremes
    assert_equal(evaluate("hsl(120, 100, 90)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: 100%)"))
    assert_equal(evaluate("hsl(120, 30, 90)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: 0%)"))
    assert_equal(evaluate("hsl(120, 0, 90)"),
      evaluate("scale-color(hsl(120, 30, 90), $saturation: -100%)"))
  end

  def test_scale_color_tests_types
    assert_error_message("$color: \"foo\" is not a color for `scale-color'", "scale-color(foo, $red: 10%)")
    # HSL
    assert_error_message("$saturation: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $saturation: foo)")
    assert_error_message("$lightness: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $lightness: foo)")
    # RGB
    assert_error_message("$red: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $red: foo)")
    assert_error_message("$green: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $green: foo)")
    assert_error_message("$blue: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $blue: foo)")
    # Alpha
    assert_error_message("$alpha: \"foo\" is not a number for `scale-color'",
      "scale-color(blue, $alpha: foo)")
  end

  def test_scale_color_argument_errors
    # Range
    assert_error_message("$saturation: Amount 101% must be between -100% and 100% for `scale-color'",
      "scale-color(blue, $saturation: 101%)")
    assert_error_message("$red: Amount -101% must be between -100% and 100% for `scale-color'",
      "scale-color(blue, $red: -101%)")
    assert_error_message("$alpha: Amount -101% must be between -100% and 100% for `scale-color'",
      "scale-color(blue, $alpha: -101%)")

    # Unit
    assert_error_message("Expected $saturation to have a unit of % but got 80 for `scale-color'",
      "scale-color(blue, $saturation: 80)")
    assert_error_message("Expected $alpha to have a unit of % but got 0.5 for `scale-color'",
      "scale-color(blue, $alpha: 0.5)")

    # Unknown argument
    assert_error_message("Unknown argument $hue (80%) for `scale-color'", "scale-color(blue, $hue: 80%)")

    # Non-keyword arg
    assert_error_message("10px is not a keyword argument for `scale_color'", "scale-color(blue, 10px)")

    # HSL/RGB
    assert_error_message("Cannot specify HSL and RGB values for a color at the same time for `scale-color'",
      "scale-color(blue, $lightness: 10%, $red: 20%)");
  end

  def test_change_color
    # HSL
    assert_equal(evaluate("hsl(195, 30, 90)"),
      evaluate("change-color(hsl(120, 30, 90), $hue: 195deg)"))
    assert_equal(evaluate("hsl(120, 50, 90)"),
      evaluate("change-color(hsl(120, 30, 90), $saturation: 50%)"))
    assert_equal(evaluate("hsl(120, 30, 40)"),
      evaluate("change-color(hsl(120, 30, 90), $lightness: 40%)"))
    # RGB
    assert_equal(evaluate("rgb(123, 20, 30)"),
      evaluate("change-color(rgb(10, 20, 30), $red: 123)"))
    assert_equal(evaluate("rgb(10, 234, 30)"),
      evaluate("change-color(rgb(10, 20, 30), $green: 234)"))
    assert_equal(evaluate("rgb(10, 20, 198)"),
      evaluate("change-color(rgb(10, 20, 30), $blue: 198)"))
    # Alpha
    assert_equal(evaluate("rgba(10, 20, 30, 0.76)"),
      evaluate("change-color(rgb(10, 20, 30), $alpha: 0.76)"))

    # HSL composability
    assert_equal(evaluate("hsl(56, 30, 47)"),
      evaluate("change-color(hsl(120, 30, 90), $hue: 56deg, $lightness: 47%)"))
    assert_equal(evaluate("hsla(56, 30, 47, 0.9)"),
      evaluate("change-color(hsl(120, 30, 90), $hue: 56deg, $lightness: 47%, $alpha: 0.9)"))
  end

  def test_change_color_tests_types
    assert_error_message("$color: \"foo\" is not a color for `change-color'", "change-color(foo, $red: 10%)")
    # HSL
    assert_error_message("$saturation: \"foo\" is not a number for `change-color'",
      "change-color(blue, $saturation: foo)")
    assert_error_message("$lightness: \"foo\" is not a number for `change-color'",
      "change-color(blue, $lightness: foo)")
    # RGB
    assert_error_message("$red: \"foo\" is not a number for `change-color'", "change-color(blue, $red: foo)")
    assert_error_message("$green: \"foo\" is not a number for `change-color'", "change-color(blue, $green: foo)")
    assert_error_message("$blue: \"foo\" is not a number for `change-color'", "change-color(blue, $blue: foo)")
    # Alpha
    assert_error_message("$alpha: \"foo\" is not a number for `change-color'", "change-color(blue, $alpha: foo)")
  end

  def test_change_color_argument_errors
    # Range
    assert_error_message("Saturation 101% must be between 0% and 100% for `change-color'",
      "change-color(blue, $saturation: 101%)")
    assert_error_message("Lightness 101% must be between 0% and 100% for `change-color'",
      "change-color(blue, $lightness: 101%)")
    assert_error_message("Red value -1 must be between 0 and 255 for `change-color'",
      "change-color(blue, $red: -1)")
    assert_error_message("Green value 256 must be between 0 and 255 for `change-color'",
      "change-color(blue, $green: 256)")
    assert_error_message("Blue value 500 must be between 0 and 255 for `change-color'",
      "change-color(blue, $blue: 500)")

    # Unknown argument
    assert_error_message("Unknown argument $hoo (80%) for `change-color'", "change-color(blue, $hoo: 80%)")

    # Non-keyword arg
    assert_error_message("10px is not a keyword argument for `change_color'", "change-color(blue, 10px)")

    # HSL/RGB
    assert_error_message("Cannot specify HSL and RGB values for a color at the same time for `change-color'",
      "change-color(blue, $lightness: 10%, $red: 120)");
  end

  def test_ie_hex_str
    assert_equal("#FFAA11CC", evaluate('ie-hex-str(#aa11cc)'))
    assert_equal("#FFAA11CC", evaluate('ie-hex-str(#a1c)'))
    assert_equal("#FFAA11CC", evaluate('ie-hex-str(#A1c)'))
    assert_equal("#80FF0000", evaluate('ie-hex-str(rgba(255, 0, 0, 0.5))'))
  end

  def test_mix
    assert_equal("#7f007f", evaluate("mix(#f00, #00f)"))
    assert_equal("#7f7f7f", evaluate("mix(#f00, #0ff)"))
    assert_equal("#7f9055", evaluate("mix(#f70, #0aa)"))
    assert_equal("#3f00bf", evaluate("mix(#f00, #00f, 25%)"))
    assert_equal("rgba(63, 0, 191, 0.75)", evaluate("mix(rgba(255, 0, 0, 0.5), #00f)"))
    assert_equal("red", evaluate("mix(#f00, #00f, 100%)"))
    assert_equal("blue", evaluate("mix(#f00, #00f, 0%)"))
    assert_equal("rgba(255, 0, 0, 0.5)", evaluate("mix(#f00, transparentize(#00f, 1))"))
    assert_equal("rgba(0, 0, 255, 0.5)", evaluate("mix(transparentize(#f00, 1), #00f)"))
    assert_equal("red", evaluate("mix(#f00, transparentize(#00f, 1), 100%)"))
    assert_equal("blue", evaluate("mix(transparentize(#f00, 1), #00f, 0%)"))
    assert_equal("rgba(0, 0, 255, 0)", evaluate("mix(#f00, transparentize(#00f, 1), 0%)"))
    assert_equal("rgba(255, 0, 0, 0)", evaluate("mix(transparentize(#f00, 1), #00f, 100%)"))
    assert_equal("rgba(255, 0, 0, 0)", evaluate("mix($color1: transparentize(#f00, 1), $color2: #00f, $weight: 100%)"))
  end

  def test_mix_deprecated_arg_name
    assert_warning <<WARNING do
DEPRECATION WARNING: The `$color-1' argument for `mix()' has been renamed to `$color1'.
DEPRECATION WARNING: The `$color-2' argument for `mix()' has been renamed to `$color2'.
WARNING
      assert_equal("rgba(255, 0, 0, 0)",
        evaluate("mix($color-1: transparentize(#f00, 1), $color-2: #00f, $weight: 100%)"))
    end

    assert_warning <<WARNING do
DEPRECATION WARNING: The `$color-1' argument for `mix()' has been renamed to `$color1'.
DEPRECATION WARNING: The `$color-2' argument for `mix()' has been renamed to `$color2'.
WARNING
      assert_equal("rgba(0, 0, 255, 0.5)",
        evaluate("mix($color-1: transparentize(#f00, 1), $color-2: #00f)"))
    end

    assert_warning <<WARNING do
DEPRECATION WARNING: The `$color_1' argument for `mix()' has been renamed to `$color1'.
DEPRECATION WARNING: The `$color_2' argument for `mix()' has been renamed to `$color2'.
WARNING
      assert_equal("rgba(0, 0, 255, 0.5)",
        evaluate("mix($color_1: transparentize(#f00, 1), $color_2: #00f)"))
    end
  end

  def test_mix_tests_types
    assert_error_message("$color1: \"foo\" is not a color for `mix'", "mix(\"foo\", #f00, 10%)")
    assert_error_message("$color2: \"foo\" is not a color for `mix'", "mix(#f00, \"foo\", 10%)")
    assert_error_message("$weight: \"foo\" is not a number for `mix'", "mix(#f00, #baf, \"foo\")")
  end

  def test_mix_tests_bounds
    assert_error_message("Weight -0.001 must be between 0% and 100% for `mix'",
      "mix(#123, #456, -0.001)")
    assert_error_message("Weight 100.001 must be between 0% and 100% for `mix'",
      "mix(#123, #456, 100.001)")
  end

  def test_grayscale
    assert_equal("#bbbbbb", evaluate("grayscale(#abc)"))
    assert_equal("gray", evaluate("grayscale(#f00)"))
    assert_equal("gray", evaluate("grayscale(#00f)"))
    assert_equal("white", evaluate("grayscale(white)"))
    assert_equal("black", evaluate("grayscale(black)"))
    assert_equal("black", evaluate("grayscale($color: black)"))

    assert_equal("grayscale(2)", evaluate("grayscale(2)"))
    assert_equal("grayscale(-5px)", evaluate("grayscale(-5px)"))
  end

  def tets_grayscale_tests_types
    assert_error_message("$color: \"foo\" is not a color for `grayscale'", "grayscale(\"foo\")")
  end

  def test_complement
    assert_equal("#ccbbaa", evaluate("complement(#abc)"))
    assert_equal("cyan", evaluate("complement(red)"))
    assert_equal("red", evaluate("complement(cyan)"))
    assert_equal("white", evaluate("complement(white)"))
    assert_equal("black", evaluate("complement(black)"))
    assert_equal("black", evaluate("complement($color: black)"))
  end

  def tets_complement_tests_types
    assert_error_message("$color: \"foo\" is not a color for `complement'", "complement(\"foo\")")
  end

  def test_invert
    assert_equal("#112233", evaluate("invert(#edc)"))
    assert_equal("rgba(245, 235, 225, 0.5)", evaluate("invert(rgba(10, 20, 30, 0.5))"))
    assert_equal("invert(20%)", evaluate("invert(20%)"))
  end

  def test_invert_tests_types
    assert_error_message("$color: \"foo\" is not a color for `invert'", "invert(\"foo\")")
  end

  def test_unquote
    assert_equal('foo', evaluate('unquote("foo")'))
    assert_equal('foo', evaluate('unquote(foo)'))
    assert_equal('foo', evaluate('unquote($string: foo)'))
  end

  def test_quote
    assert_equal('"foo"', evaluate('quote(foo)'))
    assert_equal('"foo"', evaluate('quote("foo")'))
    assert_equal('"foo"', evaluate('quote($string: "foo")'))
  end

  def test_quote_tests_type
    assert_error_message("$string: #ff0000 is not a string for `quote'", "quote(#f00)")
  end

  def test_str_length
    assert_equal('3', evaluate('str-length(foo)'))
  end

  def test_str_length_requires_a_string
    assert_error_message("$string: #ff0000 is not a string for `str-length'", "str-length(#f00)")
  end

  def test_str_insert
    assert_equal('Xabcd', evaluate('str-insert(abcd, X, 0)'))
    assert_equal('Xabcd', evaluate('str-insert(abcd, X, 1)'))
    assert_equal('abcXd', evaluate('str-insert(abcd, X, 4)'))
    assert_equal('abcdX', evaluate('str-insert(abcd, X, 100)'))
    assert_equal('Xabcd', evaluate('str-insert(abcd, X, -100)'))
    assert_equal('aXbcd', evaluate('str-insert(abcd, X, -4)'))
    assert_equal('abcdX', evaluate('str-insert(abcd, X, -1)'))
  end

  def test_str_insert_maintains_quote_of_primary_string
    assert_equal('"Xfoo"', evaluate('str-insert("foo", X, 1)'))
    assert_equal('"Xfoo"', evaluate('str-insert("foo", "X", 1)'))
    assert_equal('Xfoo', evaluate('str-insert(foo, "X", 1)'))
  end

  def test_str_insert_asserts_types
    assert_error_message("$string: #ff0000 is not a string for `str-insert'", "str-insert(#f00, X, 1)")
    assert_error_message("$insert: #ff0000 is not a string for `str-insert'", "str-insert(foo, #f00, 1)")
    assert_error_message("$index: #ff0000 is not a number for `str-insert'", "str-insert(foo, X, #f00)")
    assert_error_message("Expected $index to be unitless but got 10px for `str-insert'", "str-insert(foo, X, 10px)")
  end

  def test_str_index
    assert_equal('1', evaluate('str-index(abcd, a)'))
    assert_equal('1', evaluate('str-index(abcd, ab)'))
    assert_equal(Sass::Script::Value::Null.new, perform('str-index(abcd, X)'))
    assert_equal('3', evaluate('str-index(abcd, c)'))
  end

  def test_str_index_asserts_types
    assert_error_message("$string: #ff0000 is not a string for `str-index'", "str-index(#f00, X)")
    assert_error_message("$substring: #ff0000 is not a string for `str-index'", "str-index(asdf, #f00)")
  end

  def test_to_lower_case
    assert_equal('abcd', evaluate('to-lower-case(ABCD)'))
    assert_equal('"abcd"', evaluate('to-lower-case("ABCD")'))
    assert_error_message("$string: #ff0000 is not a string for `to-lower-case'", "to-lower-case(#f00)")
  end

  def test_to_upper_case
    assert_equal('ABCD', evaluate('to-upper-case(abcd)'))
    assert_equal('"ABCD"', evaluate('to-upper-case("abcd")'))
    assert_error_message("$string: #ff0000 is not a string for `to-upper-case'", "to-upper-case(#f00)")
  end

  def test_str_slice
    assert_equal('bc',   evaluate('str-slice(abcd,2,3)'))    # in the middle of the string
    assert_equal('a',    evaluate('str-slice(abcd,1,1)'))    # when start = end
    assert_equal('ab',   evaluate('str-slice(abcd,1,2)'))    # for completeness
    assert_equal('abcd', evaluate('str-slice(abcd,1,4)'))    # at the end points
    assert_equal('abcd', evaluate('str-slice(abcd,0,4)'))    # when start is before the start of the string
    assert_equal('abcd', evaluate('str-slice(abcd,1,100)'))  # when end is past the end of the string
    assert_equal('',     evaluate('str-slice(abcd,2,1)'))    # when end is before start
    assert_equal('"bc"', evaluate('str-slice("abcd",2,3)'))  # when used with a quoted string
    assert_equal('bcd',  evaluate('str-slice(abcd,2)'))      # when end is omitted, you get the remainder of the string
    assert_equal('cd',   evaluate('str-slice(abcd,-2)'))     # when start is negative, it counts from the beginning
    assert_equal('bc',   evaluate('str-slice(abcd,2,-2)'))   # when end is negative it counts in from the end
    assert_equal('',     evaluate('str-slice(abcd,3,-3)'))   # when end is negative and comes before the start
    assert_equal('bc',   evaluate('str-slice(abcd,-3,-2)'))  # when both are negative
    assert_error_message("$string: #ff0000 is not a string for `str-slice'", "str-slice(#f00,2,3)")
    assert_error_message("$start-at: #ff0000 is not a number for `str-slice'", "str-slice(abcd,#f00,3)")
    assert_error_message("$end-at: #ff0000 is not a number for `str-slice'", "str-slice(abcd,2,#f00)")
    assert_error_message("Expected $end-at to be unitless but got 3px for `str-slice'", "str-slice(abcd,2,3px)")
    assert_error_message("Expected $start-at to be unitless but got 2px for `str-slice'", "str-slice(abcd,2px,3)")
  end

  def test_user_defined_function
    assert_equal("I'm a user-defined string!", evaluate("user_defined()"))
  end

  def test_user_defined_function_with_preceding_underscore
    assert_equal("I'm another user-defined string!", evaluate("_preceding_underscore()"))
    assert_equal("I'm another user-defined string!", evaluate("-preceding-underscore()"))
  end

  def test_user_defined_function_using_environment
    environment = env('variable' => Sass::Script::Value::String.new('The variable'))
    assert_equal("The variable", evaluate("fetch_the_variable()", environment))
  end

  def test_options_on_new_values_fails
    assert_error_message(<<MSG, "call-options-on-new-value()")
The #options attribute is not set on this Sass::Script::Value::String.
  This error is probably occurring because #to_s was called
  on this value within a custom Sass function without first
  setting the #options attribute.
MSG
  end

  def test_type_of
    assert_equal("string", evaluate("type-of(\"asdf\")"))
    assert_equal("string", evaluate("type-of(asdf)"))
    assert_equal("number", evaluate("type-of(1px)"))
    assert_equal("bool", evaluate("type-of(true)"))
    assert_equal("color", evaluate("type-of(#fff)"))
    assert_equal("color", evaluate("type-of($value: #fff)"))
    assert_equal("null", evaluate("type-of(null)"))
    assert_equal("list", evaluate("type-of(1 2 3)"))
    assert_equal("list", evaluate("type-of((1, 2, 3))"))
    assert_equal("list", evaluate("type-of(())"))
    assert_equal("map", evaluate("type-of((foo: bar))"))
  end

  def test_feature_exists
    assert_raises ArgumentError do
      Sass.add_feature("my-test-feature")
    end
    Sass.add_feature("-my-test-feature")
    assert_equal("true", evaluate("feature-exists(-my-test-feature)"))
    assert_equal("false", evaluate("feature-exists(whatisthisidontevenknow)"))
    assert_equal("true", evaluate("feature-exists($feature: -my-test-feature)"))
  ensure
    Sass::Features::KNOWN_FEATURES.delete("-my-test-feature")
  end

  def test_unit
    assert_equal(%Q{""}, evaluate("unit(100)"))
    assert_equal(%Q{"px"}, evaluate("unit(100px)"))
    assert_equal(%Q{"em*px"}, evaluate("unit(10px * 5em)"))
    assert_equal(%Q{"em*px"}, evaluate("unit(5em * 10px)"))
    assert_equal(%Q{"em/rem"}, evaluate("unit(10px * 5em / 30cm / 1rem)"))
    assert_equal(%Q{"em*vh/cm*rem"}, evaluate("unit(10vh * 5em / 30cm / 1rem)"))
    assert_equal(%Q{"px"}, evaluate("unit($number: 100px)"))
    assert_error_message("$number: #ff0000 is not a number for `unit'", "unit(#f00)")
  end

  def test_unitless
    assert_equal(%Q{true}, evaluate("unitless(100)"))
    assert_equal(%Q{false}, evaluate("unitless(100px)"))
    assert_equal(%Q{false}, evaluate("unitless($number: 100px)"))
    assert_error_message("$number: #ff0000 is not a number for `unitless'", "unitless(#f00)")
  end

  def test_comparable
    assert_equal(%Q{true}, evaluate("comparable(2px, 1px)"))
    assert_equal(%Q{true}, evaluate("comparable(10cm, 3mm)"))
    assert_equal(%Q{false}, evaluate("comparable(100px, 3em)"))
    assert_equal(%Q{false}, evaluate("comparable($number1: 100px, $number2: 3em)"))
  end

  def test_comparable_deprecated_arg_name
    assert_warning <<WARNING do
DEPRECATION WARNING: The `$number-1' argument for `comparable()' has been renamed to `$number1'.
DEPRECATION WARNING: The `$number-2' argument for `comparable()' has been renamed to `$number2'.
WARNING
      assert_equal("false", evaluate("comparable($number-1: 100px, $number-2: 3em)"))
    end

    assert_warning <<WARNING do
DEPRECATION WARNING: The `$number_1' argument for `comparable()' has been renamed to `$number1'.
DEPRECATION WARNING: The `$number_2' argument for `comparable()' has been renamed to `$number2'.
WARNING
      assert_equal("false", evaluate("comparable($number_1: 100px, $number_2: 3em)"))
    end
  end

  def test_comparable_checks_types
    assert_error_message("$number1: #ff0000 is not a number for `comparable'", "comparable(#f00, 1px)")
    assert_error_message("$number2: #ff0000 is not a number for `comparable'", "comparable(1px, #f00)")
  end

  def test_length
    assert_equal("5", evaluate("length(1 2 3 4 5)"))
    assert_equal("4", evaluate("length((foo, bar, baz, bip))"))
    assert_equal("3", evaluate("length((foo, bar, baz bip))"))
    assert_equal("3", evaluate("length((foo, bar, (baz, bip)))"))
    assert_equal("1", evaluate("length(#f00)"))
    assert_equal("0", evaluate("length(())"))
    assert_equal("4", evaluate("length(1 2 () 3)"))

    assert_equal("2", evaluate("length((foo: bar, bar: baz))"))
  end

  def test_nth
    assert_equal("1", evaluate("nth(1 2 3, 1)"))
    assert_equal("2", evaluate("nth(1 2 3, 2)"))
    assert_equal("3", evaluate("nth(1 2 3, -1)"))
    assert_equal("1", evaluate("nth(1 2 3, -3)"))
    assert_equal("3", evaluate("nth((1, 2, 3), 3)"))
    assert_equal("3", evaluate("nth($list: (1, 2, 3), $n: 3)"))
    assert_equal("foo", evaluate("nth(foo, 1)"))
    assert_equal("bar baz", evaluate("nth(foo (bar baz) bang, 2)"))
    assert_error_message("List index 0 must be a non-zero integer for `nth'", "nth(foo, 0)")
    assert_error_message("List index is -10 but list is only 1 item long for `nth'", "nth(foo, -10)")
    assert_error_message("List index 1.5 must be a non-zero integer for `nth'", "nth(foo, 1.5)")
    assert_error_message("List index is 5 but list is only 4 items long for `nth'", "nth(1 2 3 4, 5)")
    assert_error_message("List index is 2 but list is only 1 item long for `nth'", "nth(foo, 2)")
    assert_error_message("List index is 1 but list has no items for `nth'", "nth((), 1)")
    assert_error_message("$n: \"foo\" is not a number for `nth'", "nth(1 2 3, foo)")

    assert_equal("foo bar", evaluate("nth((foo: bar, bar: baz), 1)"))
    assert_equal("bar baz", evaluate("nth((foo: bar, bar: baz), 2)"))
  end

  def test_set_nth
    assert_equal("a 2 3", evaluate("set-nth(1 2 3, 1, a)"))
    assert_equal("1 a 3", evaluate("set-nth(1 2 3, 2, a)"))
    assert_equal("1 2 a", evaluate("set-nth(1 2 3, -1, a)"))
    assert_equal("a 2 3", evaluate("set-nth(1 2 3, -3, a)"))
    assert_equal("a 2 3", evaluate("set-nth($list: 1 2 3, $n: -3, $value: a)"))
    assert_equal("1, 2, a", evaluate("set-nth((1, 2, 3), 3, a)"))
    assert_equal("a", evaluate("set-nth(foo, 1, a)"))
    assert_equal("foo, a b, baz", evaluate("set-nth((foo, bar, baz), 2, (a b))"))
    assert_error_message("List index 0 must be a non-zero integer for `set-nth'", "set-nth(foo, 0, a)")
    assert_error_message("List index is -10 but list is only 1 item long for `set-nth'", "set-nth(foo, -10, a)")
    assert_error_message("List index 1.5 must be a non-zero integer for `set-nth'", "set-nth(foo, 1.5, a)")
    assert_error_message("List index is 5 but list is only 4 items long for `set-nth'", "set-nth(1 2 3 4, 5, a)")
    assert_error_message("List index is 2 but list is only 1 item long for `set-nth'", "set-nth(foo, 2, a)")
    assert_error_message("List index is 1 but list has no items for `set-nth'", "set-nth((), 1, a)")
    assert_error_message("$n: \"foo\" is not a number for `set-nth'", "set-nth(1 2 3, foo, a)")
  end

  def test_join
    assert_equal("1 2 3", evaluate("join(1 2, 3)"))
    assert_equal("1 2 3", evaluate("join(1, 2 3)"))
    assert_equal("1 2 3 4", evaluate("join(1 2, 3 4)"))
    assert_equal("true", evaluate("(1 2 3 4) == join(1 2, 3 4)"))
    assert_equal("false", evaluate("(1 2 (3 4)) == join(1 2, 3 4)"))
    assert_equal("1, 2, 3", evaluate("join((1, 2), 3)"))
    assert_equal("1, 2, 3", evaluate("join(1, (2, 3))"))
    assert_equal("1, 2, 3, 4", evaluate("join((1, 2), (3, 4))"))
    assert_equal("true", evaluate("(1, 2, 3, 4) == join((1, 2), (3, 4))"))
    assert_equal("false", evaluate("(1, 2, (3, 4)) == join((1, 2), (3, 4))"))

    assert_equal("1 2", evaluate("join(1, 2)"))
    assert_equal("1 2 3 4", evaluate("join(1 2, (3, 4))"))
    assert_equal("1, 2, 3, 4", evaluate("join((1, 2), 3 4)"))

    assert_equal("1 2", evaluate("join(1, 2, auto)"))
    assert_equal("1, 2, 3, 4", evaluate("join(1 2, 3 4, comma)"))
    assert_equal("1 2 3 4", evaluate("join((1, 2), (3, 4), space)"))
    assert_equal("1, 2", evaluate("join(1, 2, comma)"))

    assert_equal("1 2", evaluate("join(1 2, ())"))
    assert_equal("1, 2", evaluate("join((1, 2), ())"))
    assert_equal("true", evaluate("(1 2) == join(1 2, ())"))
    assert_equal("true", evaluate("(1, 2) == join((1, 2), ())"))
    assert_equal("false", evaluate("(1 2 ()) == join(1 2, ())"))
    assert_equal("false", evaluate("(1, 2, ()) == join((1, 2), ())"))

    assert_equal("1 2", evaluate("join((), 1 2)"))
    assert_equal("1, 2", evaluate("join((), (1, 2))"))
    assert_equal("true", evaluate("(1 2) == join((), 1 2)"))
    assert_equal("true", evaluate("(1, 2) == join((), (1, 2))"))
    assert_equal("false", evaluate("(1 2 ()) == join((), 1 2)"))
    assert_equal("false", evaluate("(1, 2, ()) == join((), (1, 2))"))

    assert_error_message("Separator name must be space, comma, or auto for `join'", "join(1, 2, baboon)")
    assert_error_message("$separator: 12 is not a string for `join'", "join(1, 2, 12)")

    assert_equal("foo bar, bar baz, baz bip, bip bop",
      perform("join((foo: bar, bar: baz), (baz: bip, bip: bop))").to_sass)
    assert_equal("(foo bar) (bar baz) (baz bip) (bip bop)",
      perform("join((foo: bar, bar: baz), (baz: bip, bip: bop), space)").to_sass)
    assert_equal("foo bar (baz bip) (bip bop)",
      perform("join(foo bar, (baz: bip, bip: bop))").to_sass)
    assert_equal("foo bar, bar baz, bip, bop",
      perform("join((foo: bar, bar: baz), bip bop)").to_sass)
    assert_equal("baz bip, bip bop",
      perform("join((), (baz: bip, bip: bop))").to_sass)
    assert_equal("foo bar, bar baz",
      perform("join((foo: bar, bar: baz), ())").to_sass)
  end

  def test_append
    assert_equal("1 2 3", evaluate("append(1 2, 3)"))
    assert_equal("1 2 3 4", evaluate("append(1 2, 3 4)"))
    assert_equal("false", evaluate("(1 2 3 4) == append(1 2, 3 4)"))
    assert_equal("true", evaluate("(1 2 (3 4)) == append(1 2, 3 4)"))
    assert_equal("1, 2, 3", evaluate("append((1, 2), 3)"))
    assert_equal("1, 2, 3, 4", evaluate("append((1, 2), (3, 4))"))
    assert_equal("false", evaluate("(1, 2, 3, 4) == append((1, 2), (3, 4))"))
    assert_equal("true", evaluate("(1, 2, (3, 4)) == append((1, 2), (3, 4))"))

    assert_equal("1 2", evaluate("append(1, 2)"))
    assert_equal("1 2 3, 4", evaluate("append(1 2, (3, 4))"))
    assert_equal("true", evaluate("(1 2 (3, 4)) == append(1 2, (3, 4))"))
    assert_equal("1, 2, 3 4", evaluate("append((1, 2), 3 4)"))
    assert_equal("true", evaluate("(1, 2, 3 4) == append((1, 2), 3 4)"))

    assert_equal("1 2", evaluate("append(1, 2, auto)"))
    assert_equal("1, 2, 3 4", evaluate("append(1 2, 3 4, comma)"))
    assert_equal("1 2 3, 4", evaluate("append((1, 2), (3, 4), space)"))
    assert_equal("1, 2", evaluate("append(1, 2, comma)"))

    assert_equal("1 2", evaluate("append(1 2, ())"))
    assert_equal("1, 2", evaluate("append((1, 2), ())"))
    assert_equal("true", evaluate("(1 2 ()) == append(1 2, ())"))
    assert_equal("true", evaluate("(1, 2, ()) == append((1, 2), ())"))

    assert_equal("1 2", evaluate("append((), 1 2)"))
    assert_equal("1, 2", evaluate("append((), (1, 2))"))
    assert_equal("false", evaluate("(1 2) == append((), 1 2)"))
    assert_equal("true", evaluate("(1 2) == nth(append((), 1 2), 1)"))

    assert_error_message("Separator name must be space, comma, or auto for `append'", "append(1, 2, baboon)")
    assert_error_message("$separator: 12 is not a string for `append'", "append(1, 2, 12)")

    assert_equal("1 2 (foo: bar)", perform("append(1 2, (foo: bar))").to_sass)
    assert_equal("foo bar, bar baz, 1", perform("append((foo: bar, bar: baz), 1)").to_sass)
    assert_equal("foo bar, bar baz, (baz: bip)",
      perform("append((foo: bar, bar: baz), (baz: bip))").to_sass)
  end

  def test_zip
    assert_equal("1 3 5, 2 4 6", evaluate("zip(1 2, 3 4, 5 6)"))
    assert_equal("1 4 7, 2 5 8", evaluate("zip(1 2 3, 4 5 6, 7 8)"))
    assert_equal("1 2 3", evaluate("zip(1, 2, 3)"))
    assert_equal("(foo bar) 1 3, (bar baz) 2 4",
      perform("zip((foo: bar, bar: baz), 1 2, 3 4)").to_sass)
  end

  def test_index
    assert_equal("1", evaluate("index(1px solid blue, 1px)"))
    assert_equal("2", evaluate("index(1px solid blue, solid)"))
    assert_equal("3", evaluate("index(1px solid blue, #00f)"))
    assert_equal("1", evaluate("index(1px, 1px)"))
    assert_equal("false", evaluate("index(1px solid blue, 1em)"))
    assert_equal("false", evaluate("index(1px solid blue, notfound)"))
    assert_equal("false", evaluate("index(1px, #00f)"))

    assert_equal("1", evaluate("index((foo: bar, bar: baz), (foo bar))"))
    assert_equal("false", evaluate("index((foo: bar, bar: baz), (foo: bar))"))
  end

  def test_index_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "== false" on
the return value. For example, instead of "@if index(...) == false", just write
"@if index(...)".
WARNING
      assert_equal("true", evaluate("index(1, 2 3 4) == false"))
    end

    assert_warning(<<WARNING) do
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "!= null" on
the return value.
WARNING
      assert_equal("true", evaluate("index(1, 2 3 4) != null"))
    end

    assert_warning(<<WARNING) do
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "== false" on
the return value. For example, instead of "@if index(...) == false", just write
"@if index(...)".
WARNING
      assert_equal("true", evaluate("false == index(1, 2 3 4)"))
    end

    assert_warning(<<WARNING) do
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "!= null" on
the return value.
WARNING
      assert_equal("true", evaluate("null != index(1, 2 3 4)"))
    end
  end

  def test_index_deprecation_warning_is_only_emitted_once_per_call
    assert_warning(<<WARNING) do
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "== false" on
the return value. For example, instead of "@if index(...) == false", just write
"@if index(...)".
        on line 3 of test_index_deprecation_warning_is_only_emitted_once_per_call_inline.scss
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "== false" on
the return value. For example, instead of "@if index(...) == false", just write
"@if index(...)".
        on line 6 of test_index_deprecation_warning_is_only_emitted_once_per_call_inline.scss
WARNING
      render(<<SCSS)
@for $i from 1 to 10 {
  $var1: index(1, 2 3 4);
  $var2: $var1 == false;
  $var3: $var1 != null;
}
$var4: index(1, 2 3 4) == false;
SCSS
    end
  end

  def test_list_separator
    assert_equal("space", evaluate("list-separator(1 2 3 4 5)"))
    assert_equal("comma", evaluate("list-separator((foo, bar, baz, bip))"))
    assert_equal("comma", evaluate("list-separator((foo, bar, baz bip))"))
    assert_equal("comma", evaluate("list-separator((foo, bar, (baz, bip)))"))
    assert_equal("space", evaluate("list-separator(#f00)"))
    assert_equal("space", evaluate("list-separator(())"))
    assert_equal("space", evaluate("list-separator(1 2 () 3)"))

    assert_equal("comma", evaluate("list-separator((foo: bar, bar: baz))"))
  end

  def test_if
    assert_equal("1px", evaluate("if(true, 1px, 2px)"))
    assert_equal("2px", evaluate("if(false, 1px, 2px)"))
    assert_equal("2px", evaluate("if(null, 1px, 2px)"))
    assert_equal("1px", evaluate("if(true, 1px, $broken)"))
    assert_equal("1px", evaluate("if(false, $broken, 1px)"))
    assert_equal("1px", evaluate("if(false, $if-true: $broken, $if-false: 1px)"))
    assert_equal("1px", evaluate("if(true, $if-true: 1px, $if-false: $broken)"))
    assert_equal(<<CSS, render(<<SCSS))
.if {
  result: yay; }
CSS
.if {
  $something: yay;
  result: if(true, $if-true: $something, $if-false: $broken);
}
SCSS
    assert_equal(<<CSS, render(<<SCSS))
.if {
  result: 1px; }
CSS
.if {
  $splat: 1px, 2px;
  result: if(true, $splat...);
}
SCSS
  end

  def test_counter
    assert_equal("counter(foo)", evaluate("counter(foo)"))
    assert_equal('counter(item,".")', evaluate('counter(item, ".")'))
    assert_equal('counter(item,".")', evaluate('counter(item,".")'))
  end

  def test_counters
    assert_equal("counters(foo)", evaluate("counters(foo)"))
    assert_equal('counters(item,".")', evaluate('counters(item, ".")'))
    assert_equal('counters(item,".")', evaluate('counters(item,".")'))
  end

  def test_keyword_args_rgb
    assert_equal(%Q{white}, evaluate("rgb($red: 255, $green: 255, $blue: 255)"))
  end

  def test_keyword_args_rgba
    assert_equal(%Q{rgba(255, 255, 255, 0.5)}, evaluate("rgba($red: 255, $green: 255, $blue: 255, $alpha: 0.5)"))
    assert_equal(%Q{rgba(255, 255, 255, 0.5)}, evaluate("rgba($color: #fff, $alpha: 0.5)"))
  end

  def test_keyword_args_rgba_with_extra_args
    evaluate("rgba($red: 255, $green: 255, $blue: 255, $alpha: 0.5, $extra: error)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Function rgba doesn't have an argument named $extra", e.message)
  end

  def test_keyword_args_must_have_signature
    evaluate("no-kw-args($fake: value)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Function no_kw_args doesn't support keyword arguments", e.message)
  end

  def test_keyword_args_with_missing_argument
    evaluate("rgb($red: 255, $green: 255)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Function rgb requires an argument named $blue", e.message)
  end

  def test_keyword_args_with_extra_argument
    evaluate("rgb($red: 255, $green: 255, $blue: 255, $purple: 255)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Function rgb doesn't have an argument named $purple", e.message)
  end

  def test_keyword_args_with_positional_and_keyword_argument
    evaluate("rgb(255, 255, 255, $red: 255)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Function rgb was passed argument $red both by position and by name", e.message)
  end

  def test_keyword_args_with_keyword_before_positional_argument
    evaluate("rgb($red: 255, 255, 255)")
    flunk("Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal("Positional arguments must come before keyword arguments.", e.message)
  end

  def test_only_var_args
    assert_equal "only-var-args(2px, 3px, 4px)", evaluate("only-var-args(1px, 2px, 3px)")
  end

  def test_only_kw_args
    assert_equal "only-kw-args(a, b, c)", evaluate("only-kw-args($a: 1, $b: 2, $c: 3)")
  end

  def test_unique_id
    last_id, current_id = nil, evaluate("unique-id()")

    50.times do
      last_id, current_id = current_id, evaluate("unique-id()")
      assert_match(/u[a-z0-9]{8}/, current_id)
      assert_not_equal last_id, current_id
    end
  end

  def test_map_get
    assert_equal "1", evaluate("map-get((foo: 1, bar: 2), foo)")
    assert_equal "2", evaluate("map-get((foo: 1, bar: 2), bar)")
    assert_equal "null", perform("map-get((foo: 1, bar: 2), baz)").to_sass
    assert_equal "null", perform("map-get((), foo)").to_sass
  end

  def test_map_get_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-get is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal "1", evaluate("map-get((foo 1) (bar 2), foo)")
    end
  end

  def test_map_get_checks_type
    assert_error_message("$map: 12 is not a map for `map-get'", "map-get(12, bar)")
  end

  def test_map_merge
    assert_equal("(foo: 1, bar: 2, baz: 3)",
      perform("map-merge((foo: 1, bar: 2), (baz: 3))").to_sass)
    assert_equal("(foo: 1, bar: 2)",
      perform("map-merge((), (foo: 1, bar: 2))").to_sass)
    assert_equal("(foo: 1, bar: 2)",
      perform("map-merge((foo: 1, bar: 2), ())").to_sass)
  end

  def test_map_merge_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-merge is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("(foo: 1, bar: 2, baz: 3)",
        perform("map-merge((foo 1, bar 2), (baz: 3))").to_sass)
    end

    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-merge is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("(baz: 3, foo: 1, bar: 2)",
        perform("map-merge((baz: 3), (foo 1, bar 2))").to_sass)
    end
  end

  def test_map_merge_checks_type
    assert_error_message("$map1: 12 is not a map for `map-merge'", "map-merge(12, (foo: 1))")
    assert_error_message("$map2: 12 is not a map for `map-merge'", "map-merge((foo: 1), 12)")
  end

  def test_map_remove
    assert_equal("(foo: 1, baz: 3)",
      perform("map-remove((foo: 1, bar: 2, baz: 3), bar)").to_sass)
    assert_equal("()", perform("map-remove((), foo)").to_sass)
  end

  def test_map_remove_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-remove is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("(foo: 1, baz: 3)",
        perform("map-remove((foo 1, bar 2, baz 3), bar)").to_sass)
    end
  end

  def test_map_remove_checks_type
    assert_error_message("$map: 12 is not a map for `map-remove'", "map-remove(12, foo)")
  end

  def test_map_keys
    assert_equal("foo, bar",
      perform("map-keys((foo: 1, bar: 2))").to_sass)
    assert_equal("()", perform("map-keys(())").to_sass)
  end

  def test_map_keys_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-keys is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("foo, bar",
        perform("map-keys((foo 1, bar 2))").to_sass)
    end
  end

  def test_map_keys_checks_type
    assert_error_message("$map: 12 is not a map for `map-keys'", "map-keys(12)")
  end

  def test_map_values
    assert_equal("1, 2", perform("map-values((foo: 1, bar: 2))").to_sass)
    assert_equal("1, 2, 2",
      perform("map-values((foo: 1, bar: 2, baz: 2))").to_sass)
    assert_equal("()", perform("map-values(())").to_sass)
  end

  def test_map_values_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-values is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("1, 2", perform("map-values((foo 1, bar 2))").to_sass)
    end
  end

  def test_map_values_checks_type
    assert_error_message("$map: 12 is not a map for `map-values'", "map-values(12)")
  end

  def test_map_has_key
    assert_equal "true", evaluate("map-has-key((foo: 1, bar: 1), foo)")
    assert_equal "false", evaluate("map-has-key((foo: 1, bar: 1), baz)")
    assert_equal "false", evaluate("map-has-key((), foo)")
  end

  def test_map_has_key_deprecation_warning
    assert_warning(<<WARNING) do
DEPRECATION WARNING: Passing lists of pairs to map-has-key is deprecated and will
be removed in future versions of Sass. Use Sass maps instead. For details, see
http://sass-lang.com/docs/yardoc/file.SASS_REFERENCE.html#maps.
WARNING
      assert_equal("true", evaluate("map-has-key((foo 1, bar 1), foo)"))
    end
  end

  def test_map_has_key_checks_type
    assert_error_message("$map: 12 is not a map for `map-has-key'", "map-has-key(12, foo)")
  end

  def test_keywords
    # The actual functionality is tested in tests where real arglists are passed.
    assert_error_message("$args: 12 is not a variable argument list for `keywords'", "keywords(12)")
    assert_error_message(
      "$args: (1 2 3) is not a variable argument list for `keywords'", "keywords(1 2 3)")
  end

  def test_partial_list_of_pairs_doesnt_work_as_a_map
    assert_raises(Sass::SyntaxError) {evaluate("map-get((foo bar, baz bang, bip), 1)")}
    assert_raises(Sass::SyntaxError) {evaluate("map-get((foo bar, baz bang, bip bap bop), 1)")}
    assert_raises(Sass::SyntaxError) {evaluate("map-get((foo bar), 1)")}
  end

  def test_assert_unit
    ctx = Sass::Script::Functions::EvaluationContext.new(Sass::Environment.new(nil, {}))
    ctx.assert_unit Sass::Script::Value::Number.new(10, ["px"], []), "px"
    ctx.assert_unit Sass::Script::Value::Number.new(10, [], []), nil

    begin
      ctx.assert_unit Sass::Script::Value::Number.new(10, [], []), "px"
      fail
    rescue ArgumentError => e
      assert_equal "Expected 10 to have a unit of px", e.message
    end

    begin
      ctx.assert_unit Sass::Script::Value::Number.new(10, ["px"], []), nil
      fail
    rescue ArgumentError => e
      assert_equal "Expected 10px to be unitless", e.message
    end

    begin
      ctx.assert_unit Sass::Script::Value::Number.new(10, [], []), "px", "arg"
      fail
    rescue ArgumentError => e
      assert_equal "Expected $arg to have a unit of px but got 10", e.message
    end

    begin
      ctx.assert_unit Sass::Script::Value::Number.new(10, ["px"], []), nil, "arg"
      fail
    rescue ArgumentError => e
      assert_equal "Expected $arg to be unitless but got 10px", e.message
    end
  end

  def test_call_with_positional_arguments
    assert_equal evaluate("lighten(blue, 5%)"), evaluate("call(lighten, blue, 5%)")
  end

  def test_call_with_keyword_arguments
    assert_equal(
      evaluate("lighten($color: blue, $amount: 5%)"),
      evaluate("call(lighten, $color: blue, $amount: 5%)"))
  end

  def test_call_with_keyword_and_positional_arguments
    assert_equal(
      evaluate("lighten(blue, $amount: 5%)"),
      evaluate("call(lighten, blue, $amount: 5%)"))
  end

  def test_call_with_dynamic_name
    assert_equal(
      evaluate("lighten($color: blue, $amount: 5%)"),
      evaluate("call($fn, $color: blue, $amount: 5%)",
        env("fn" => Sass::Script::String.new("lighten"))))
  end

  def test_call_uses_local_scope
    assert_equal <<CSS, render(<<SCSS)
.first-scope {
  a: local; }

.second-scope {
  a: global; }
CSS
@function foo() {@return global}

.first-scope {
  @function foo() {@return local}
  a: call(foo);
}

.second-scope {
  a: call(foo);
}
SCSS
  end

  def test_call_unknown_function
    assert_equal evaluate("unknown(red, blue)"), evaluate("call(unknown, red, blue)")
  end

  def test_call_with_non_string_argument
    assert_error_message "$name: 3px is not a string for `call'", "call(3px)"
  end

  def test_errors_in_called_function
    assert_error_message "$color: 3px is not a color for `lighten'", "call(lighten, 3px, 5%)"
  end

  def test_variable_exists
    assert_equal <<CSS, render(<<SCSS)
.test {
  false: false;
  true: true;
  true: true;
  true: true;
  true: true; }
CSS
$global-var: has-value;
.test {
  false: variable-exists(foo);
  $foo: has-value;
  true: variable-exists(foo);
  true: variable-exists($name: foo);
  true: variable-exists(global-var);
  true: variable-exists($name: global-var);
}
SCSS
  end

  def test_variable_exists_checks_type
    assert_error_message("$name: 1 is not a string for `variable-exists'", "variable-exists(1)")
  end

  def test_global_variable_exists
    assert_equal <<CSS, render(<<SCSS)
.test {
  false: false;
  false: false;
  true: true;
  true: true;
  false: false;
  true: true;
  true: true; }
CSS
$g: something;
$h: null;
$false: global-variable-exists(foo);
$true: global-variable-exists(g);
$named: global-variable-exists($name: g);
.test {
  $foo: locally-defined;
  false: global-variable-exists(foo);
  false: global-variable-exists(foo2);
  true: global-variable-exists(g);
  true: global-variable-exists(h);
  false: $false;
  true: $true;
  true: $named;
}
SCSS
  end

  def test_global_variable_exists_checks_type
    assert_error_message("$name: 1 is not a string for `global-variable-exists'",
      "global-variable-exists(1)")
  end

  def test_function_exists
    # built-ins
    assert_equal "true", evaluate("function-exists(lighten)")
    # with named argument
    assert_equal "true", evaluate("function-exists($name: lighten)")
    # user-defined
    assert_equal <<CSS, render(<<SCSS)
.test {
  foo-exists: true;
  bar-exists: false; }
CSS
@function foo() { @return "foo" }
.test {
  foo-exists: function-exists(foo);
  bar-exists: function-exists(bar);
}
SCSS
  end

  def test_function_exists_checks_type
    assert_error_message("$name: 1 is not a string for `function-exists'", "function-exists(1)")
  end

  def test_mixin_exists
    assert_equal "false", evaluate("mixin-exists(foo)")
    # with named argument
    assert_equal "false", evaluate("mixin-exists($name: foo)")
    assert_equal <<CSS, render(<<SCSS)
.test {
  foo-exists: true;
  bar-exists: false; }
CSS
@mixin foo() { foo: exists }
.test {
  foo-exists: mixin-exists(foo);
  bar-exists: mixin-exists(bar);
}
SCSS
  end

  def test_mixin_exists_checks_type
    assert_error_message("$name: 1 is not a string for `mixin-exists'", "mixin-exists(1)")
  end

  def test_inspect
    assert_equal "()", evaluate("inspect(())")
    assert_equal "null", evaluate("inspect(null)")
    assert_equal "1px null 3px", evaluate("inspect(1px null 3px)")
    assert_equal "(a: 1, b: 2)", evaluate("inspect((a: 1, b: 2))")
  end

  def test_random
    Sass::Script::Functions.random_seed = 1
    assert_equal "0.41702", evaluate("random()")
    assert_equal "13", evaluate("random(100)")
  end

  def test_random_works_without_a_seed
    if Sass::Script::Functions.instance_variable_defined?("@random_number_generator")
      Sass::Script::Functions.send(:remove_instance_variable, "@random_number_generator")
    end

    result = perform("random()")
    assert_kind_of Sass::Script::Number, result
    assert result.value >= 0, "Random number was below 0"
    assert result.value <= 1, "Random number was above 1"
  end

  def test_random_with_limit_one
    # Passing 1 as the limit should always return 1, since limit calls return
    # integers from 1 to the argument, so when the argument is 1, its a predicatble
    # outcome
    assert "1", evaluate("random(1)")
  end

  def test_random_with_limit_too_low
    assert_error_message("$limit 0 must be greater than or equal to 1 for `random'", "random(0)")
  end

  def test_random_with_non_integer_limit
    assert_error_message("Expected $limit to be an integer but got 1.5 for `random'", "random(1.5)")
  end

  # This could *possibly* fail, but exceedingly unlikely
  def test_random_is_semi_unique
    if Sass::Script::Functions.instance_variable_defined?("@random_number_generator")
      Sass::Script::Functions.send(:remove_instance_variable, "@random_number_generator")
    end
    assert_not_equal evaluate("random()"), evaluate("random()")
  end

  ## Regression Tests

  def test_inspect_nested_empty_lists
    assert_equal "() ()", evaluate("inspect(() ())")
  end

  def test_saturation_bounds
    assert_equal "#fbfdff", evaluate("hsl(hue(#fbfdff), saturation(#fbfdff), lightness(#fbfdff))")
  end

  private
  def env(hash = {}, parent = nil)
    env = Sass::Environment.new(parent)
    hash.each {|k, v| env.set_var(k, v)}
    env
  end

  def evaluate(value, environment = env)
    result = perform(value, environment)
    assert_kind_of Sass::Script::Value::Base, result
    return result.to_s
  end

  def perform(value, environment = env)
    Sass::Script::Parser.parse(value, 0, 0).perform(environment)
  end

  def render(sass, options = {})
    options[:syntax] ||= :scss
    munge_filename options
    options[:importer] ||= MockImporter.new
    Sass::Engine.new(sass, options).render
  end

  def assert_error_message(message, value)
    evaluate(value)
    flunk("Error message expected but not raised: #{message}")
  rescue Sass::SyntaxError => e
    assert_equal(message, e.message)
  end
end
