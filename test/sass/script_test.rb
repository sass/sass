#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class SassScriptTest < Test::Unit::TestCase
  include Sass::Script

  def test_color_checks_input
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([1, 2, -1])}
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([256, 2, 3])}
  end
end
