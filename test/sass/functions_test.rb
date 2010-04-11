#!/usr/bin/env ruby
require 'test/unit'
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/script'

module Sass::Script::Functions::UserFunctions
  def call_options_on_new_literal
    str = Sass::Script::String.new("foo")
    str.options[:foo]
    str
  end

  def user_defined
    Sass::Script::String.new("I'm a user-defined string!")
  end

  def _preceding_underscore
    Sass::Script::String.new("I'm another user-defined string!")
  end
end

class SassFunctionTest < Test::Unit::TestCase
  # Tests taken from:
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-h-rotating-b.htm
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-values-b.htm
  File.read(File.dirname(__FILE__) + "/data/hsl-rgb.txt").split("\n\n").each do |chunk|
    hsls, rgbs = chunk.strip.split("====")
    hsls.strip.split("\n").zip(rgbs.strip.split("\n")) do |hsl, rgb|
      hsl_method = "test_hsl: #{hsl} = #{rgb}"
      define_method(hsl_method) do
        assert_equal(evaluate(rgb), evaluate(hsl))
      end

      rgb_to_hsl_method = "test_rgb_to_hsl: #{rgb} = #{hsl}"
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

  def test_hsl_checks_bounds
    assert_error_message("Saturation -114 must be between 0% and 100% for `hsl'", "hsl(10, -114, 12)");
    assert_error_message("Lightness 256 must be between 0% and 100% for `hsl'", "hsl(10, 10, 256%)");
  end

  def test_hsl_checks_types
    assert_error_message("\"foo\" is not a number for `hsl'", "hsl(\"foo\", 10, 12)");
    assert_error_message("\"foo\" is not a number for `hsl'", "hsl(10, \"foo\", 12)");
    assert_error_message("\"foo\" is not a number for `hsl'", "hsl(10, 10, \"foo\")");
  end

  def test_hsla
    assert_equal "rgba(51, 204, 204, 0.4)", evaluate("hsla(180, 60%, 50%, 0.4)")
    assert_equal "#33cccc", evaluate("hsla(180, 60%, 50%, 1)")
    assert_equal "rgba(51, 204, 204, 0)", evaluate("hsla(180, 60%, 50%, 0)")
  end

  def test_hsla_checks_bounds
    assert_error_message("Saturation -114 must be between 0% and 100% for `hsla'", "hsla(10, -114, 12, 1)");
    assert_error_message("Lightness 256 must be between 0% and 100% for `hsla'", "hsla(10, 10, 256%, 0)");
    assert_error_message("Alpha channel -0.1 must be between 0 and 1 for `hsla'", "hsla(10, 10, 10, -0.1)");
    assert_error_message("Alpha channel 1.1 must be between 0 and 1 for `hsla'", "hsla(10, 10, 10, 1.1)");
  end

  def test_hsla_checks_types
    assert_error_message("\"foo\" is not a number for `hsla'", "hsla(\"foo\", 10, 12, 0.3)");
    assert_error_message("\"foo\" is not a number for `hsla'", "hsla(10, \"foo\", 12, 0)");
    assert_error_message("\"foo\" is not a number for `hsla'", "hsla(10, 10, \"foo\", 1)");
    assert_error_message("\"foo\" is not a number for `hsla'", "hsla(10, 10, 10, \"foo\")");
  end

  def test_percentage
    assert_equal("50%",  evaluate("percentage(.5)"))
    assert_equal("100%", evaluate("percentage(1)"))
    assert_equal("25%",  evaluate("percentage(25px / 100px)"))
  end

  def test_percentage_checks_types
    assert_error_message("25px is not a unitless number for `percentage'", "percentage(25px)")
    assert_error_message("#cccccc is not a unitless number for `percentage'", "percentage(#ccc)")
    assert_error_message("\"string\" is not a unitless number for `percentage'", %Q{percentage("string")})
  end

  def test_round
    assert_equal("5",   evaluate("round(4.8)"))
    assert_equal("5px", evaluate("round(4.8px)"))
    assert_equal("5px", evaluate("round(5.49px)"))

    assert_error_message("#cccccc is not a number for `round'", "round(#ccc)")
  end

  def test_floor
    assert_equal("4",   evaluate("floor(4.8)"))
    assert_equal("4px", evaluate("floor(4.8px)"))

    assert_error_message("\"foo\" is not a number for `floor'", "floor(\"foo\")")
  end

  def test_ceil
    assert_equal("5",   evaluate("ceil(4.1)"))
    assert_equal("5px", evaluate("ceil(4.8px)"))

    assert_error_message("\"a\" is not a number for `ceil'", "ceil(\"a\")")
  end

  def test_abs
    assert_equal("5",   evaluate("abs(-5)"))
    assert_equal("5px", evaluate("abs(-5px)"))
    assert_equal("5",   evaluate("abs(5)"))
    assert_equal("5px", evaluate("abs(5px)"))

    assert_error_message("#aaaaaa is not a number for `abs'", "abs(#aaa)")
  end

  def test_rgb
    assert_equal("#123456", evaluate("rgb(18, 52, 86)"))
    assert_equal("#beaded", evaluate("rgb(190, 173, 237)"))
    assert_equal("#00ff7f", evaluate("rgb(0, 255, 127)"))
  end

  def test_rgb_percent
    assert_equal("#123456", evaluate("rgb(7.1%, 20.4%, 34%)"))
    assert_equal("#beaded", evaluate("rgb(74.7%, 173, 93%)"))
    assert_equal("#beaded", evaluate("rgb(190, 68%, 237)"))
    assert_equal("#00ff7f", evaluate("rgb(0%, 100%, 50%)"))
  end

  def test_rgb_tests_bounds
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgb'",
      "rgb(256, 1, 1)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgb'",
      "rgb(1, 256, 1)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgb'",
      "rgb(1, 1, 256)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgb'",
      "rgb(1, 256, 257)")
    assert_error_message("Color value -1 must be between 0 and 255 inclusive for `rgb'",
      "rgb(-1, 1, 1)")
  end

  def test_rgb_test_percent_bounds
    assert_error_message("Color value 100.1% must be between 0% and 100% inclusive for `rgb'",
      "rgb(100.1%, 0, 0)")
    assert_error_message("Color value -0.1% must be between 0% and 100% inclusive for `rgb'",
      "rgb(0, -0.1%, 0)")
    assert_error_message("Color value 101% must be between 0% and 100% inclusive for `rgb'",
      "rgb(0, 0, 101%)")
  end

  def test_rgb_tests_types
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(\"foo\", 10, 12)");
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(10, \"foo\", 12)");
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(10, 10, \"foo\")");
  end

  def test_rgba
    assert_equal("rgba(18, 52, 86, 0.5)", evaluate("rgba(18, 52, 86, 0.5)"))
    assert_equal("#beaded", evaluate("rgba(190, 173, 237, 1)"))
    assert_equal("rgba(0, 255, 127, 0)", evaluate("rgba(0, 255, 127, 0)"))
  end

  def test_rgb_tests_bounds
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgba'",
      "rgba(256, 1, 1, 0.3)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgba'",
      "rgba(1, 256, 1, 0.3)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgba'",
      "rgba(1, 1, 256, 0.3)")
    assert_error_message("Color value 256 must be between 0 and 255 inclusive for `rgba'",
      "rgba(1, 256, 257, 0.3)")
    assert_error_message("Color value -1 must be between 0 and 255 inclusive for `rgba'",
      "rgba(-1, 1, 1, 0.3)")
    assert_error_message("Alpha channel -0.2 must be between 0 and 1 inclusive for `rgba'",
      "rgba(1, 1, 1, -0.2)")
    assert_error_message("Alpha channel 1.2 must be between 0 and 1 inclusive for `rgba'",
      "rgba(1, 1, 1, 1.2)")
  end

  def test_rgba_tests_types
    assert_error_message("\"foo\" is not a number for `rgba'", "rgba(\"foo\", 10, 12, 0.2)");
    assert_error_message("\"foo\" is not a number for `rgba'", "rgba(10, \"foo\", 12, 0.1)");
    assert_error_message("\"foo\" is not a number for `rgba'", "rgba(10, 10, \"foo\", 0)");
    assert_error_message("\"foo\" is not a number for `rgba'", "rgba(10, 10, 10, \"foo\")");
  end

  def test_rgba_with_color
    assert_equal "rgba(16, 32, 48, 0.5)", evaluate("rgba(#102030, 0.5)")
    assert_equal "rgba(0, 0, 255, 0.5)", evaluate("rgba(blue, 0.5)")
  end

  def test_rgba_with_color_tests_types
    assert_error_message("\"foo\" is not a color for `rgba'", "rgba(\"foo\", 0.2)");
    assert_error_message("\"foo\" is not a number for `rgba'", "rgba(blue, \"foo\")");
  end

  def test_rgba_tests_num_args
    assert_error_message("wrong number of arguments (0 for 4) for `rgba'", "rgba()");
    assert_error_message("wrong number of arguments (1 for 4) for `rgba'", "rgba(blue)");
    assert_error_message("wrong number of arguments (3 for 4) for `rgba'", "rgba(1, 2, 3)");
    assert_error_message("wrong number of arguments (5 for 4) for `rgba'", "rgba(1, 2, 3, 0.4, 5)");
  end

  def test_red
    assert_equal("18", evaluate("red(#123456)"))
  end

  def test_red_exception
    assert_error_message("12 is not a color for `red'", "red(12)")
  end

  def test_green
    assert_equal("52", evaluate("green(#123456)"))
  end

  def test_green_exception
    assert_error_message("12 is not a color for `green'", "green(12)")
  end

  def test_blue
    assert_equal("86", evaluate("blue(#123456)"))
  end

  def test_blue_exception
    assert_error_message("12 is not a color for `blue'", "blue(12)")
  end

  def test_hue
    assert_equal("18deg", evaluate("hue(hsl(18, 50%, 20%))"))
  end

  def test_hue_exception
    assert_error_message("12 is not a color for `hue'", "hue(12)")
  end

  def test_saturation
    assert_equal("52%", evaluate("saturation(hsl(20, 52%, 20%))"))
    assert_equal("52%", evaluate("saturation(hsl(20, 52, 20%))"))
  end

  def test_saturation_exception
    assert_error_message("12 is not a color for `saturation'", "saturation(12)")
  end

  def test_lightness
    assert_equal("86%", evaluate("lightness(hsl(120, 50%, 86%))"))
    assert_equal("86%", evaluate("lightness(hsl(120, 50%, 86))"))
  end

  def test_lightness_exception
    assert_error_message("12 is not a color for `lightness'", "lightness(12)")
  end

  def test_alpha
    assert_equal("1", evaluate("alpha(#123456)"))
    assert_equal("0.34", evaluate("alpha(rgba(0, 1, 2, 0.34))"))
    assert_equal("0", evaluate("alpha(hsla(0, 1, 2, 0))"))
  end

  def test_alpha_exception
    assert_error_message("12 is not a color for `alpha'", "alpha(12)")
  end

  def test_opacify
    assert_equal("rgba(0, 0, 0, 0.75)", evaluate("opacify(rgba(0, 0, 0, 0.5), 0.25)"))
    assert_equal("rgba(0, 0, 0, 0.3)", evaluate("opacify(rgba(0, 0, 0, 0.2), 0.1)"))
    assert_equal("rgba(0, 0, 0, 0.7)", evaluate("fade-in(rgba(0, 0, 0, 0.2), 0.5px)"))
    assert_equal("black", evaluate("fade_in(rgba(0, 0, 0, 0.2), 0.8)"))
    assert_equal("black", evaluate("opacify(rgba(0, 0, 0, 0.2), 1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("opacify(rgba(0, 0, 0, 0.2), 0%)"))
  end

  def test_opacify_tests_bounds
    assert_error_message("Amount -0.001 must be between 0 and 1 for `opacify'",
      "opacify(rgba(0, 0, 0, 0.2), -0.001)")
    assert_error_message("Amount 1.001 must be between 0 and 1 for `opacify'",
      "opacify(rgba(0, 0, 0, 0.2), 1.001)")
  end

  def test_opacify_tests_types
    assert_error_message("\"foo\" is not a color for `opacify'", "opacify(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `opacify'", "opacify(#fff, \"foo\")")
  end

  def test_transparentize
    assert_equal("rgba(0, 0, 0, 0.3)", evaluate("transparentize(rgba(0, 0, 0, 0.5), 0.2)"))
    assert_equal("rgba(0, 0, 0, 0.1)", evaluate("transparentize(rgba(0, 0, 0, 0.2), 0.1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("fade-out(rgba(0, 0, 0, 0.5), 0.3px)"))
    assert_equal("rgba(0, 0, 0, 0)", evaluate("fade_out(rgba(0, 0, 0, 0.2), 0.2)"))
    assert_equal("rgba(0, 0, 0, 0)", evaluate("transparentize(rgba(0, 0, 0, 0.2), 1)"))
    assert_equal("rgba(0, 0, 0, 0.2)", evaluate("transparentize(rgba(0, 0, 0, 0.2), 0)"))
  end

  def test_transparentize_tests_bounds
    assert_error_message("Amount -0.001 must be between 0 and 1 for `transparentize'",
      "transparentize(rgba(0, 0, 0, 0.2), -0.001)")
    assert_error_message("Amount 1.001 must be between 0 and 1 for `transparentize'",
      "transparentize(rgba(0, 0, 0, 0.2), 1.001)")
  end

  def test_transparentize_tests_types
    assert_error_message("\"foo\" is not a color for `transparentize'", "transparentize(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `transparentize'", "transparentize(#fff, \"foo\")")
  end

  def test_lighten
    assert_equal("#4d4d4d", evaluate("lighten(hsl(0, 0, 0), 30%)"))
    assert_equal("#ee0000", evaluate("lighten(#800, 20%)"))
    assert_equal("white", evaluate("lighten(#fff, 20%)"))
    assert_equal("white", evaluate("lighten(#800, 100%)"))
    assert_equal("#880000", evaluate("lighten(#800, 0%)"))
    assert_equal("rgba(238, 0, 0, 0.5)", evaluate("lighten(rgba(136, 0, 0, 0.5), 20%)"))
  end

  def test_lighten_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `lighten'",
      "lighten(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `lighten'",
      "lighten(#123, 100.001)")
  end

  def test_lighten_tests_types
    assert_error_message("\"foo\" is not a color for `lighten'", "lighten(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `lighten'", "lighten(#fff, \"foo\")")
  end

  def test_darken
    assert_equal("#ff6a00", evaluate("darken(hsl(25, 100, 80), 30%)"))
    assert_equal("#220000", evaluate("darken(#800, 20%)"))
    assert_equal("black", evaluate("darken(#000, 20%)"))
    assert_equal("black", evaluate("darken(#800, 100%)"))
    assert_equal("#880000", evaluate("darken(#800, 0%)"))
    assert_equal("rgba(34, 0, 0, 0.5)", evaluate("darken(rgba(136, 0, 0, 0.5), 20%)"))
  end

  def test_darken_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `darken'",
      "darken(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `darken'",
      "darken(#123, 100.001)")
  end

  def test_darken_tests_types
    assert_error_message("\"foo\" is not a color for `darken'", "darken(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `darken'", "darken(#fff, \"foo\")")
  end

  def test_saturate
    assert_equal("#d9f2d9", evaluate("saturate(hsl(120, 30, 90), 20%)"))
    assert_equal("#9e3f3f", evaluate("saturate(#855, 20%)"))
    assert_equal("black", evaluate("saturate(#000, 20%)"))
    assert_equal("white", evaluate("saturate(#fff, 20%)"))
    assert_equal("#33ff33", evaluate("saturate(#8a8, 100%)"))
    assert_equal("#88aa88", evaluate("saturate(#8a8, 0%)"))
    assert_equal("rgba(158, 63, 63, 0.5)", evaluate("saturate(rgba(136, 85, 85, 0.5), 20%)"))
  end

  def test_saturate_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `saturate'",
      "saturate(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `saturate'",
      "saturate(#123, 100.001)")
  end

  def test_saturate_tests_types
    assert_error_message("\"foo\" is not a color for `saturate'", "saturate(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `saturate'", "saturate(#fff, \"foo\")")
  end

  def test_desaturate
    assert_equal("#e3e8e3", evaluate("desaturate(hsl(120, 30, 90), 20%)"))
    assert_equal("#726b6b", evaluate("desaturate(#855, 20%)"))
    assert_equal("black", evaluate("desaturate(#000, 20%)"))
    assert_equal("white", evaluate("desaturate(#fff, 20%)"))
    assert_equal("#999999", evaluate("desaturate(#8a8, 100%)"))
    assert_equal("#88aa88", evaluate("desaturate(#8a8, 0%)"))
    assert_equal("rgba(114, 107, 107, 0.5)", evaluate("desaturate(rgba(136, 85, 85, 0.5), 20%)"))
  end

  def test_desaturate_tests_bounds
    assert_error_message("Amount -0.001 must be between 0% and 100% for `desaturate'",
      "desaturate(#123, -0.001)")
    assert_error_message("Amount 100.001 must be between 0% and 100% for `desaturate'",
      "desaturate(#123, 100.001)")
  end

  def test_desaturate_tests_types
    assert_error_message("\"foo\" is not a color for `desaturate'", "desaturate(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `desaturate'", "desaturate(#fff, \"foo\")")
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
  end

  def test_adjust_hue_tests_types
    assert_error_message("\"foo\" is not a color for `adjust-hue'", "adjust-hue(\"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `adjust-hue'", "adjust-hue(#fff, \"foo\")")
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
  end

  def test_mix_tests_types
    assert_error_message("\"foo\" is not a color for `mix'", "mix(\"foo\", #f00, 10%)")
    assert_error_message("\"foo\" is not a color for `mix'", "mix(#f00, \"foo\", 10%)")
    assert_error_message("\"foo\" is not a number for `mix'", "mix(#f00, #baf, \"foo\")")
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
  end

  def tets_grayscale_tests_types
    assert_error_message("\"foo\" is not a color for `grayscale'", "grayscale(\"foo\")")
  end

  def test_complement
    assert_equal("#ccbbaa", evaluate("complement(#abc)"))
    assert_equal("aqua", evaluate("complement(red)"))
    assert_equal("red", evaluate("complement(aqua)"))
    assert_equal("white", evaluate("complement(white)"))
    assert_equal("black", evaluate("complement(black)"))
  end

  def tets_complement_tests_types
    assert_error_message("\"foo\" is not a color for `complement'", "complement(\"foo\")")
  end

  def test_unquote
    assert_equal('foo', evaluate('unquote("foo")'))
    assert_equal('foo', evaluate('unquote(foo)'))
  end

  def test_unquote_tests_type
    assert_error_message("#ff0000 is not a string for `unquote'", "unquote(#f00)")
  end

  def test_quote
    assert_equal('"foo"', evaluate('quote(foo)'))
    assert_equal('"foo"', evaluate('quote("foo")'))
  end

  def test_quote_tests_type
    assert_error_message("#ff0000 is not a string for `quote'", "quote(#f00)")
  end

  def test_user_defined_function
    assert_equal("I'm a user-defined string!", evaluate("user_defined()"))
  end

  def test_user_defined_function_with_preceding_underscore
    assert_equal("I'm another user-defined string!", evaluate("_preceding_underscore()"))
    assert_equal("I'm another user-defined string!", evaluate("-preceding-underscore()"))
  end

  def test_options_on_new_literals_fails
    assert_error_message(<<MSG, "call-options-on-new-literal()")
The #options attribute is not set on this Sass::Script::String.
  This error is probably occurring because #to_s was called
  on this literal within a custom Sass function without first
  setting the #option attribute.
MSG
  end

  def test_type_of
    assert_equal("string", evaluate("type-of(\"asdf\")"))
    assert_equal("string", evaluate("type-of(asdf)"))
    assert_equal("number", evaluate("type-of(1px)"))
    assert_equal("bool", evaluate("type-of(true)"))
    assert_equal("color", evaluate("type-of(#fff)"))
  end

  def test_unit
    assert_equal(%Q{""}, evaluate("unit(100)"))
    assert_equal(%Q{"px"}, evaluate("unit(100px)"))
    assert_equal(%Q{"em*px"}, evaluate("unit(10px * 5em)"))
    assert_equal(%Q{"em*px"}, evaluate("unit(5em * 10px)"))
    assert_equal(%Q{"em*px/cm*rem"}, evaluate("unit(10px * 5em / 30cm / 1rem)"))
    assert_error_message("#ff0000 is not a number for `unit'", "unit(#f00)")
  end

  def test_unitless
    assert_equal(%Q{true}, evaluate("unitless(100)"))
    assert_equal(%Q{false}, evaluate("unitless(100px)"))
    assert_error_message("#ff0000 is not a number for `unitless'", "unitless(#f00)")
  end

  def test_comparable
    assert_equal(%Q{true}, evaluate("comparable(2px, 1px)"))
    assert_equal(%Q{true}, evaluate("comparable(10cm, 3mm)"))
    assert_equal(%Q{false}, evaluate("comparable(100px, 3em)"))
    assert_error_message("#ff0000 is not a number for `comparable'", "comparable(#f00, 1px)")
    assert_error_message("#ff0000 is not a number for `comparable'", "comparable(1px, #f00)")
  end

  private

  def evaluate(value)
    Sass::Script::Parser.parse(value, 0, 0).perform(Sass::Environment.new).to_s
  end

  def perform(value)
    Sass::Script::Parser.parse(value, 0, 0).perform(Sass::Environment.new)
  end

  def assert_error_message(message, value)
    evaluate(value)
    flunk("Error message expected but not raised: #{message}")
  rescue Sass::SyntaxError => e
    assert_equal(message, e.message)
  end

end
