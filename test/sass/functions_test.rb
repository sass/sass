require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'
require 'sass/script'

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

  def test_rgb_tests_types
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(\"foo\", 10, 12)");
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(10, \"foo\", 12)");
    assert_error_message("\"foo\" is not a number for `rgb'", "rgb(10, 10, \"foo\")");
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
