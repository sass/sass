#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class SassScriptTest < Test::Unit::TestCase
  include Sass::Script

  def eval(str, environment = {})
    Sass::Script.resolve(str, 0, environment)
  end

  def test_color_checks_input
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([1, 2, -1])}
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([256, 2, 3])}
  end

  def test_string_escapes
    assert_equal '"', eval("\"\\\"\"")
    assert_equal "\\", eval("\"\\\\\"")
    assert_equal "\\02fa", eval("\"\\02fa\"")
  end

  def test_color_names
    assert_equal "white", eval("white")
    assert_equal "white", eval("#ffffff")
    assert_equal "#fffffe", eval("white - #000001")
  end
end
