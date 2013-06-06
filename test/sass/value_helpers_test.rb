#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ValueHelpersTest < Test::Unit::TestCase
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

  def test_hex_color_alpha_enforces_0_to_1
    assert_raises ArgumentError do
      hex_color("FF007F", 50)
    end
  end
end
