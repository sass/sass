require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'
require 'sass/constant'

class SassFunctionTest < Test::Unit::TestCase
  def test_hsl
    # These tests adapted from the w3c browser tests
    # http://www.w3.org/Style/CSS/Test/CSS3/Color/20070927/html4/t040204-hsl-h-rotating-b.htm
    red = [255, 0, 0]
    assert_rgb_hsl(red, [0, '100%', '50%'])
    assert_rgb_hsl(red, [-360, '100%', '50%'])
    assert_rgb_hsl(red, [360, '100%', '50%'])
    assert_rgb_hsl(red, [6120, '100%', '50%'])

    yellow = [255, 255, 0]
    assert_rgb_hsl(yellow, [60, '100%', '50%'])
    assert_rgb_hsl(yellow, [-300, '100%', '50%'])
    assert_rgb_hsl(yellow, [420, '100%', '50%'])
    assert_rgb_hsl(yellow, [-9660, '100%', '50%'])

    green = [0, 255, 0]
    assert_rgb_hsl(green, [120, '100%', '50%'])
    assert_rgb_hsl(green, [-240, '100%', '50%'])
    assert_rgb_hsl(green, [480, '100%', '50%'])
    assert_rgb_hsl(green, [99840, '100%', '50%'])

    cyan = [0, 255, 255]
    assert_rgb_hsl(cyan, [180, '100%', '50%'])
    assert_rgb_hsl(cyan, [-180, '100%', '50%'])
    assert_rgb_hsl(cyan, [540, '100%', '50%'])
    assert_rgb_hsl(cyan, [-900, '100%', '50%'])

    blue = [0, 0, 255]
    assert_rgb_hsl(blue, [240, '100%', '50%'])
    assert_rgb_hsl(blue, [-120, '100%', '50%'])
    assert_rgb_hsl(blue, [600, '100%', '50%'])
    assert_rgb_hsl(blue, [-104880, '100%', '50%'])

    purple = [255, 0, 255]
    assert_rgb_hsl(purple, [300, '100%', '50%'])
    assert_rgb_hsl(purple, [-60, '100%', '50%'])
    assert_rgb_hsl(purple, [660, '100%', '50%'])
    assert_rgb_hsl(purple, [2820, '100%', '50%'])
  end

  private

  def assert_rgb_hsl(rgb, hsl)
    assert_equal(rgb, Sass::Constant::Functions.hsl(*hsl.map(&Sass::Constant::Number.method(:new))).value)
  end
end
