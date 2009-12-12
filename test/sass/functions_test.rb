require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'
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
end

class SassFunctionTest < Test::Unit::TestCase
  # Tests taken from:
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-h-rotating-b.htm
  #   http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-values-b.htm
  File.read(File.dirname(__FILE__) + "/data/hsl-rgb.txt").split("\n\n").each do |chunk|
    hsls, rgbs = chunk.strip.split("====")
    hsls.strip.split("\n").zip(rgbs.strip.split("\n")) do |hsl, rgb|
      method = "test_hsl: #{hsl} = #{rgb}"
      define_method(method) do
        assert_equal(evaluate(rgb), evaluate(hsl))
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

  def test_user_defined_function
    assert_equal("I'm a user-defined string!", evaluate("user_defined()"))
  end

  def test_options_on_new_literals_fails
    assert_error_message(<<MSG, "call-options-on-new-literal()")
The #options attribute is not set on this Sass::Script::String.
  This error is probably occurring because #to_s was called
  on this literal within a custom Sass function without first
  setting the #option attribute.
MSG
  end

  private

  def evaluate(value)
    Sass::Script::Parser.parse(value, 0, 0).perform(Sass::Environment.new).to_s
  end

  def assert_error_message(message, value)
    evaluate(value)
    flunk("Error message expected but not raised: #{message}")
  rescue Sass::SyntaxError => e
    assert_equal(message, e.message)
  end

end
