#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ValueHelpersTest < MiniTest::Test
  include Sass::Script
  include Sass::Script::Value::Helpers

  def test_bool
    assert_same Value::Bool::TRUE, bool(true)
    assert_same Value::Bool::FALSE, bool(false)
    assert_same Value::Bool::FALSE, bool(nil)
    assert_same Value::Bool::TRUE, bool(Object.new)
  end

  def test_hex_color_with_three_digits
    color = hex_color("F07")
    assert_equal 255, color.red
    assert_equal 0, color.green
    assert_equal 119, color.blue
    assert_equal 1, color.alpha
  end

  def test_hex_color_without_hash
    color_without_hash = hex_color("FF007F")
    assert_equal 255, color_without_hash.red
    assert_equal 0, color_without_hash.green
    assert_equal 127, color_without_hash.blue
    assert_equal 1, color_without_hash.alpha
  end

  def test_hex_color_with_hash
    color_with_hash = hex_color("#FF007F")
    assert_equal 255, color_with_hash.red
    assert_equal 0, color_with_hash.green
    assert_equal 127, color_with_hash.blue
    assert_equal 1, color_with_hash.alpha
  end

  def test_malformed_hex_color
    assert_raises ArgumentError do
      hex_color("green")
    end
    assert_raises ArgumentError do
      hex_color("#abcd")
    end
  end


  def test_hex_color_with_alpha
    color_with_alpha = hex_color("FF007F", 0.5)
    assert_equal 0.5, color_with_alpha.alpha
  end

  def test_hex_color_alpha_clamps_0_to_1
    assert_equal 1, hex_color("FF007F", 50).alpha
  end

  def test_hsl_color_without_alpha
    no_alpha = hsl_color(1, 0.5, 1)
    assert_equal 1, no_alpha.hue
    assert_equal 0.5, no_alpha.saturation
    assert_equal 1, no_alpha.lightness
    assert_equal 1, no_alpha.alpha
  end

  def test_hsl_color_with_alpha
    has_alpha = hsl_color(1, 0.5, 1, 0.5)
    assert_equal 1, has_alpha.hue
    assert_equal 0.5, has_alpha.saturation
    assert_equal 1, has_alpha.lightness
    assert_equal 0.5, has_alpha.alpha
  end

  def test_rgb_color_without_alpha
    no_alpha = rgb_color(255, 0, 0)
    assert_equal 255, no_alpha.red
    assert_equal 0, no_alpha.green
    assert_equal 0, no_alpha.blue
    assert_equal 1, no_alpha.alpha
  end

  def test_rgb_color_with_alpha
    has_alpha = rgb_color(255, 255, 255, 0.5)
    assert_equal 255, has_alpha.red
    assert_equal 255, has_alpha.green
    assert_equal 255, has_alpha.blue
    assert_equal 0.5, has_alpha.alpha
  end

  def test_number
    n = number(1)
    assert_equal 1, n.value
    assert_equal "1", n.to_sass
  end

  def test_number_with_single_unit
    n = number(1, "px")
    assert_equal 1, n.value
    assert_equal "1px", n.to_sass
  end

  def test_number_with_singal_numerator_and_denominator
    ratio = number(1, "px/em")
    assert_equal "1px/em", ratio.to_sass
  end

  def test_number_with_many_numerator_and_denominator_units
    complex = number(1, "px*in/em*%")
    assert_equal "1in*px/%*em", complex.to_sass
  end

  def test_number_with_many_numerator_and_denominator_units_with_spaces
    complex = number(1, "px * in / em * %")
    assert_equal "1in*px/%*em", complex.to_sass
  end

  def test_number_with_malformed_units
    assert_raises ArgumentError do
      number(1, "px/em/%")
    end
    assert_raises ArgumentError do
      number(1, "/")
    end
    assert_raises ArgumentError do
      number(1, "px/")
    end
  end

  def test_space_list
    l = list(number(1, "px"), hex_color("#f71"), :space)
    l.options = {}
    assert_kind_of Sass::Script::Value::List, l
    assert_equal "1px #f71", l.to_sass
  end

  def test_comma_list
    l = list(number(1, "px"), hex_color("#f71"), :comma)
    l.options = {}
    assert_kind_of Sass::Script::Value::List, l
    assert_equal "1px, #f71", l.to_sass
  end

  def test_missing_list_type
    assert_raises ArgumentError do
      list(number(1, "px"), hex_color("#f71"))
    end
  end

  def test_null
    assert_kind_of Sass::Script::Value::Null, null
  end

  def test_quoted_string
    s = quoted_string("sassy string")
    s.options = {}
    assert_kind_of Sass::Script::Value::String, s
    assert_equal "sassy string", s.value
    assert_equal :string, s.type
    assert_equal '"sassy string"', s.to_sass
  end

  def test_identifier
    s = identifier("a-sass-ident")
    s.options = {}
    assert_kind_of Sass::Script::Value::String, s
    assert_equal "a-sass-ident", s.value
    assert_equal :identifier, s.type
    assert_equal "a-sass-ident", s.to_sass
  end

  def test_unquoted_string
    s = unquoted_string("a-sass-ident")
    s.options = {}
    assert_kind_of Sass::Script::Value::String, s
    assert_equal "a-sass-ident", s.value
    assert_equal :identifier, s.type
    assert_equal "a-sass-ident", s.to_sass
  end
end
