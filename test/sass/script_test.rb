#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

module Sass::Script::Functions::UserFunctions
  def assert_options(val)
    val.options[:foo]
    Sass::Script::Value::String.new("Options defined!")
  end

  def arg_error
    assert_options
  end
end

module Sass::Script::Functions
  include Sass::Script::Functions::UserFunctions
end

class SassScriptTest < MiniTest::Test
  include Sass::Script

  def test_color_clamps_input
    assert_equal 0, Sass::Script::Value::Color.new([1, 2, -1]).blue
    assert_equal 255, Sass::Script::Value::Color.new([256, 2, 3]).red
  end

  def test_color_clamps_rgba_input
    assert_equal 1, Sass::Script::Value::Color.new([1, 2, 3, 1.1]).alpha
    assert_equal 0, Sass::Script::Value::Color.new([1, 2, 3, -0.1]).alpha
  end

  def test_color_from_hex
    assert_equal Sass::Script::Value::Color.new([0,0,0]), Sass::Script::Value::Color.from_hex('000000')
    assert_equal Sass::Script::Value::Color.new([0,0,0]), Sass::Script::Value::Color.from_hex('#000000')
  end

  def test_string_escapes
    assert_equal "'", resolve("\"'\"")
    assert_equal '"', resolve("\"\\\"\"")
    assert_equal "\\", resolve("\"\\\\\"")
    assert_equal "☃", resolve("\"\\2603\"")
    assert_equal "☃f", resolve("\"\\2603 f\"")
    assert_equal "☃x", resolve("\"\\2603x\"")
    assert_equal "\\2603", resolve("\"\\\\2603\"")

    # U+FFFD is the replacement character, "�".
    assert_equal [0xFFFD].pack("U"), resolve("\"\\0\"")
    assert_equal [0xFFFD].pack("U"), resolve("\"\\FFFFFF\"")
    assert_equal [0xFFFD].pack("U"), resolve("\"\\D800\"")
    assert_equal [0xD7FF].pack("U"), resolve("\"\\D7FF\"")
    assert_equal [0xFFFD].pack("U"), resolve("\"\\DFFF\"")
    assert_equal [0xE000].pack("U"), resolve("\"\\E000\"")
  end

  def test_string_escapes_are_resolved_before_operators
    assert_equal "true", resolve('"abc" == "\61\62\63"')
  end

  def test_string_quote
    assert_equal '"foo"', resolve_quoted('"foo"')
    assert_equal "'f\"oo'", resolve_quoted('"f\"oo"')
    assert_equal "\"f'oo\"", resolve_quoted("'f\\'oo'")
    assert_equal "\"f'o\\\"o\"", resolve_quoted("'f\\'o\"o'")
    assert_equal '"foo bar"', resolve_quoted('"foo\20 bar"')
    assert_equal '"foo\a bar"', resolve_quoted('"foo\a bar"')
    assert_equal '"x\ay"', resolve_quoted('"x\a y"')
    assert_equal '"\a  "', resolve_quoted('"\a\20"')
    assert_equal '"\a abcdef"', resolve_quoted('"\a abcdef"')
    assert_equal '"☃abcdef"', resolve_quoted('"\2603 abcdef"')
    assert_equal '"\\\\"', resolve_quoted('"\\\\"')
    assert_equal '"foobar"', resolve_quoted("\"foo\\\nbar\"")
  end

  def test_color_names
    assert_equal "white", resolve("white")
    assert_equal "#ffffff", resolve("#ffffff")
    assert_equal "#fffffe", resolve("white - #000001")
    assert_equal "transparent", resolve("transparent")
    assert_equal "transparent", resolve("rgba(0, 0, 0, 0)")
  end

  def test_rgba_color_literals
    assert_equal Sass::Script::Value::Color.new([1, 2, 3, 0.75]), eval("rgba(1, 2, 3, 0.75)")
    assert_equal "rgba(1, 2, 3, 0.75)", resolve("rgba(1, 2, 3, 0.75)")

    assert_equal Sass::Script::Value::Color.new([1, 2, 3, 0]), eval("rgba(1, 2, 3, 0)")
    assert_equal "rgba(1, 2, 3, 0)", resolve("rgba(1, 2, 3, 0)")

    assert_equal Sass::Script::Value::Color.new([1, 2, 3]), eval("rgba(1, 2, 3, 1)")
    assert_equal Sass::Script::Value::Color.new([1, 2, 3, 1]), eval("rgba(1, 2, 3, 1)")
    assert_equal "#010203", resolve("rgba(1, 2, 3, 1)")
    assert_equal "white", resolve("rgba(255, 255, 255, 1)")
  end

  def test_rgba_color_math
    assert_equal "rgba(50, 50, 100, 0.35)", resolve("rgba(1, 1, 2, 0.35) * rgba(50, 50, 50, 0.35)")
    assert_equal "rgba(52, 52, 52, 0.25)", resolve("rgba(2, 2, 2, 0.25) + rgba(50, 50, 50, 0.25)")

    assert_raise_message(Sass::SyntaxError, "Alpha channels must be equal: rgba(1, 2, 3, 0.15) + rgba(50, 50, 50, 0.75)") do
      resolve("rgba(1, 2, 3, 0.15) + rgba(50, 50, 50, 0.75)")
    end
    assert_raise_message(Sass::SyntaxError, "Alpha channels must be equal: #123456 * rgba(50, 50, 50, 0.75)") do
      resolve("#123456 * rgba(50, 50, 50, 0.75)")
    end
    assert_raise_message(Sass::SyntaxError, "Alpha channels must be equal: rgba(50, 50, 50, 0.75) / #123456") do
      resolve("rgba(50, 50, 50, 0.75) / #123456")
    end
  end

  def test_rgba_number_math
    assert_equal "rgba(49, 49, 49, 0.75)", resolve("rgba(50, 50, 50, 0.75) - 1")
    assert_equal "rgba(100, 100, 100, 0.75)", resolve("rgba(50, 50, 50, 0.75) * 2")
  end

  def test_rgba_rounding
    assert_equal "rgba(10, 1, 0, 0.12346)", resolve("rgba(10.0, 1.23456789, 0.0, 0.1234567)")
  end

  def test_compressed_colors
    assert_equal "#123456", resolve("#123456", :style => :compressed)
    assert_equal "rgba(1,2,3,0.5)", resolve("rgba(1, 2, 3, 0.5)", :style => :compressed)
    assert_equal "#123", resolve("#112233", :style => :compressed)
    assert_equal "#000", resolve("black", :style => :compressed)
    assert_equal "red", resolve("#f00", :style => :compressed)
    assert_equal "blue", resolve("blue", :style => :compressed)
    assert_equal "navy", resolve("#000080", :style => :compressed)
    assert_equal "navy #fff", resolve("#000080 white", :style => :compressed)
    assert_equal "This color is #fff", resolve('"This color is #{ white }"', :style => :compressed)
    assert_equal "transparent", resolve("rgba(0, 0, 0, 0)", :style => :compressed)
  end

  def test_compressed_comma
    # assert_equal "foo,bar,baz", resolve("foo, bar, baz", :style => :compressed)
    # assert_equal "foo,#baf,baz", resolve("foo, #baf, baz", :style => :compressed)
    assert_equal "foo,#baf,red", resolve("foo, #baf, #f00", :style => :compressed)
  end

  def test_implicit_strings
    assert_equal Sass::Script::Value::String.new("foo"), eval("foo")
    assert_equal Sass::Script::Value::String.new("foo/bar"), eval("foo/bar")
  end

  def test_basic_interpolation
    assert_equal "foo3bar", resolve("foo\#{1 + 2}bar")
    assert_equal "foo3 bar", resolve("foo\#{1 + 2} bar")
    assert_equal "foo 3bar", resolve("foo \#{1 + 2}bar")
    assert_equal "foo 3 bar", resolve("foo \#{1 + 2} bar")
    assert_equal "foo 35 bar", resolve("foo \#{1 + 2}\#{2 + 3} bar")
    assert_equal "foo 3 5 bar", resolve("foo \#{1 + 2} \#{2 + 3} bar")
    assert_equal "3bar", resolve("\#{1 + 2}bar")
    assert_equal "foo3", resolve("foo\#{1 + 2}")
    assert_equal "3", resolve("\#{1 + 2}")
  end

  def test_interpolation_in_function
    assert_equal 'flabnabbit(1foo)', resolve('flabnabbit(#{1 + "foo"})')
    assert_equal 'flabnabbit(foo 1foobaz)', resolve('flabnabbit(foo #{1 + "foo"}baz)')
    assert_equal('flabnabbit(foo 1foo2bar baz)',
      resolve('flabnabbit(foo #{1 + "foo"}#{2 + "bar"} baz)'))
  end

  def test_interpolation_near_operators
    assert_equal '3 , 7', resolve('#{1 + 2} , #{3 + 4}')
    assert_equal '3, 7', resolve('#{1 + 2}, #{3 + 4}')
    assert_equal '3 ,7', resolve('#{1 + 2} ,#{3 + 4}')
    assert_equal '3,7', resolve('#{1 + 2},#{3 + 4}')
    assert_equal '3, 7, 11', resolve('#{1 + 2}, #{3 + 4}, #{5 + 6}')
    assert_equal '3, 7, 11', resolve('3, #{3 + 4}, 11')
    assert_equal '3, 7, 11', resolve('3, 7, #{5 + 6}')

    assert_equal '3 / 7', resolve('3 / #{3 + 4}')
    assert_equal '3 /7', resolve('3 /#{3 + 4}')
    assert_equal '3/ 7', resolve('3/ #{3 + 4}')
    assert_equal '3/7', resolve('3/#{3 + 4}')

    assert_equal '3 * 7', resolve('#{1 + 2} * 7')
    assert_equal '3* 7', resolve('#{1 + 2}* 7')
    assert_equal '3 *7', resolve('#{1 + 2} *7')
    assert_equal '3*7', resolve('#{1 + 2}*7')

    assert_equal '-3', resolve('-#{1 + 2}')
    assert_equal '- 3', resolve('- #{1 + 2}')

    assert_equal '5 + 3 * 7', resolve('5 + #{1 + 2} * #{3 + 4}')
    assert_equal '5 +3 * 7', resolve('5 +#{1 + 2} * #{3 + 4}')
    assert_equal '5+3 * 7', resolve('5+#{1 + 2} * #{3 + 4}')
    assert_equal '3 * 7 + 5', resolve('#{1 + 2} * #{3 + 4} + 5')
    assert_equal '3 * 7+ 5', resolve('#{1 + 2} * #{3 + 4}+ 5')
    assert_equal '3 * 7+5', resolve('#{1 + 2} * #{3 + 4}+5')

    assert_equal '5/3 + 7', resolve('5 / (#{1 + 2} + #{3 + 4})')
    assert_equal '5/3 + 7', resolve('5 /(#{1 + 2} + #{3 + 4})')
    assert_equal '5/3 + 7', resolve('5 /( #{1 + 2} + #{3 + 4} )')
    assert_equal '3 + 7/5', resolve('(#{1 + 2} + #{3 + 4}) / 5')
    assert_equal '3 + 7/5', resolve('(#{1 + 2} + #{3 + 4})/ 5')
    assert_equal '3 + 7/5', resolve('( #{1 + 2} + #{3 + 4} )/ 5')

    assert_equal '3 + 5', resolve('#{1 + 2} + 2 + 3')
    assert_equal '3 +5', resolve('#{1 + 2} +2 + 3')
  end

  def test_string_interpolation
    assert_equal "foo bar, baz bang", resolve('"foo #{"bar"}, #{"baz"} bang"')
    assert_equal "foo bar baz bang", resolve('"foo #{"#{"ba" + "r"} baz"} bang"')
    assert_equal 'foo #{bar baz} bang', resolve('"foo \#{#{"ba" + "r"} baz} bang"')
    assert_equal 'foo #{baz bang', resolve('"foo #{"\#{" + "baz"} bang"')
    assert_equal "foo2bar", resolve('\'foo#{1 + 1}bar\'')
    assert_equal "foo2bar", resolve('"foo#{1 + 1}bar"')
    assert_equal "foo1bar5baz4bang", resolve('\'foo#{1 + "bar#{2 + 3}baz" + 4}bang\'')
  end

  def test_interpolation_in_interpolation
    assert_equal 'foo', resolve('#{#{foo}}')
    assert_equal 'foo', resolve('"#{#{foo}}"')
    assert_equal 'foo', resolve('#{"#{foo}"}')
    assert_equal 'foo', resolve('"#{"#{foo}"}"')
  end

  def test_interpolation_with_newline
    assert_equal "\nbang", resolve('"#{"\a "}bang"')
    assert_equal "\n\nbang", resolve('"#{"\a "}\a bang"')
  end

  def test_rule_interpolation
    assert_equal(<<CSS, render(<<SASS))
foo bar baz bang {
  a: b; }
CSS
foo \#{"\#{"ba" + "r"} baz"} bang
  a: b
SASS
    assert_equal(<<CSS, render(<<SASS))
foo [bar="\#{bar baz}"] bang {
  a: b; }
CSS
foo [bar="\\\#{\#{"ba" + "r"} baz}"] bang
  a: b
SASS
    assert_equal(<<CSS, render(<<SASS))
foo [bar="\#{baz"] bang {
  a: b; }
CSS
foo [bar="\#{"\\\#{" + "baz"}"] bang
  a: b
SASS
  end

  def test_inaccessible_functions
    assert_equal "send(to_s)", resolve("send(to_s)", :line => 2)
    assert_equal "public_instance_methods()", resolve("public_instance_methods()")
  end

  def test_adding_functions_directly_to_functions_module
    assert !Functions.callable?('nonexistant')
    Functions.class_eval { def nonexistant; end }
    assert Functions.callable?('nonexistant')
    Functions.send :remove_method, :nonexistant
  end

  def test_default_functions
    assert_equal "url(12)", resolve("url(12)")
    assert_equal 'blam("foo")', resolve('blam("foo")')
  end

  def test_function_results_have_options
    assert_equal "Options defined!", resolve("assert_options(abs(1))")
    assert_equal "Options defined!", resolve("assert_options(round(1.2))")
  end

  def test_funcall_requires_no_whitespace_before_lparen
    assert_equal "no-repeat 15px", resolve("no-repeat (7px + 8px)")
    assert_equal "no-repeat(15px)", resolve("no-repeat(7px + 8px)")
  end

  def test_dynamic_url
    assert_equal "url(foo-bar)", resolve("url($foo)", {}, env('foo' => Sass::Script::Value::String.new("foo-bar")))
    assert_equal "url(foo-bar baz)", resolve("url($foo $bar)", {}, env('foo' => Sass::Script::Value::String.new("foo-bar"), 'bar' => Sass::Script::Value::String.new("baz")))
    assert_equal "url(foo baz)", resolve("url(foo $bar)", {}, env('bar' => Sass::Script::Value::String.new("baz")))
    assert_equal "url(foo bar)", resolve("url(foo    bar)")
  end

  def test_url_with_interpolation
    assert_equal "url(http://sass-lang.com/images/foo-bar)", resolve("url(http://sass-lang.com/images/\#{foo-bar})")
    assert_equal 'url("http://sass-lang.com/images/foo-bar")', resolve("url('http://sass-lang.com/images/\#{foo-bar}')")
    assert_equal 'url("http://sass-lang.com/images/foo-bar")', resolve('url("http://sass-lang.com/images/#{foo-bar}")')
    assert_unquoted "url(http://sass-lang.com/images/\#{foo-bar})"
  end

  def test_hyphenated_variables
    assert_equal("a-b", resolve("$a-b", {}, env("a-b" => Sass::Script::Value::String.new("a-b"))))
  end

  def test_ruby_equality
    assert_equal eval('"foo"'), eval('"foo"')
    assert_equal eval('1'), eval('1.0')
    assert_equal eval('1 2 3.0'), eval('1 2 3')
    assert_equal eval('1, 2, 3.0'), eval('1, 2, 3')
    assert_equal eval('(1 2), (3, 4), (5 6)'), eval('(1 2), (3, 4), (5 6)')
    refute_equal eval('1, 2, 3'), eval('1 2 3')
    refute_equal eval('1'), eval('"1"')
  end

  def test_booleans
    assert_equal "true", resolve("true")
    assert_equal "false", resolve("false")
  end

  def test_null
    assert_equal "", resolve("null")
  end

  def test_boolean_ops
    assert_equal "true", resolve("true and true")
    assert_equal "true", resolve("false or true")
    assert_equal "true", resolve("true or false")
    assert_equal "true", resolve("true or true")
    assert_equal "false", resolve("false or false")
    assert_equal "false", resolve("false and true")
    assert_equal "false", resolve("true and false")
    assert_equal "false", resolve("false and false")

    assert_equal "true", resolve("not false")
    assert_equal "false", resolve("not true")
    assert_equal "true", resolve("not not true")

    assert_equal "1", resolve("false or 1")
    assert_equal "false", resolve("false and 1")
    assert_equal "2", resolve("2 or 3")
    assert_equal "3", resolve("2 and 3")

    assert_equal "true", resolve("null or true")
    assert_equal "true", resolve("true or null")
    assert_equal "", resolve("null or null")
    assert_equal "", resolve("null and true")
    assert_equal "", resolve("true and null")
    assert_equal "", resolve("null and null")

    assert_equal "true", resolve("not null")

    assert_equal "1", resolve("null or 1")
    assert_equal "", resolve("null and 1")
  end

  def test_arithmetic_ops
    assert_equal "2", resolve("1 + 1")
    assert_equal "0", resolve("1 - 1")
    assert_equal "8", resolve("2 * 4")
    assert_equal "0.5", resolve("(2 / 4)")
    assert_equal "2", resolve("(4 / 2)")

    assert_equal "-1", resolve("-1")
  end

  def test_string_ops
    assert_equal '"foo" "bar"', resolve('"foo" "bar"')
    assert_equal "true 1", resolve('true 1')
    assert_equal '"foo", "bar"', resolve("'foo' , 'bar'")
    assert_equal "true, 1", resolve('true , 1')
    assert_equal "foobar", resolve('"foo" + "bar"')
    assert_equal "\nfoo\nxyz", resolve('"\a foo" + "\axyz"')
    assert_equal "true1", resolve('true + 1')
    assert_equal '"foo"-"bar"', resolve("'foo' - 'bar'")
    assert_equal "true-1", resolve('true - 1')
    assert_equal '"foo"/"bar"', resolve('"foo" / "bar"')
    assert_equal "true/1", resolve('true / 1')

    assert_equal '-"bar"', resolve("- 'bar'")
    assert_equal "-true", resolve('- true')
    assert_equal '/"bar"', resolve('/ "bar"')
    assert_equal "/true", resolve('/ true')
  end

  def test_relational_ops
    assert_equal "false", resolve("1 > 2")
    assert_equal "false", resolve("2 > 2")
    assert_equal "true", resolve("3 > 2")
    assert_equal "false", resolve("1 >= 2")
    assert_equal "true", resolve("2 >= 2")
    assert_equal "true", resolve("3 >= 2")
    assert_equal "true", resolve("1 < 2")
    assert_equal "false", resolve("2 < 2")
    assert_equal "false", resolve("3 < 2")
    assert_equal "true", resolve("1 <= 2")
    assert_equal "true", resolve("2 <= 2")
    assert_equal "false", resolve("3 <= 2")
  end

  def test_null_ops
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null plus 1".') {eval("null + 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null minus 1".') {eval("null - 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null times 1".') {eval("null * 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null div 1".') {eval("null / 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null mod 1".') {eval("null % 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 plus null".') {eval("1 + null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 minus null".') {eval("1 - null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 times null".') {eval("1 * null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 div null".') {eval("1 / null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 mod null".') {eval("1 % null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "1 gt null".') {eval("1 > null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null lt 1".') {eval("null < 1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: "null plus null".') {eval("null + null")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid null operation: ""foo" plus null".') {eval("foo + null")}
  end

  def test_equals
    assert_equal("true", resolve('"foo" == $foo', {},
        env("foo" => Sass::Script::Value::String.new("foo"))))
    assert_equal "true", resolve("1 == 1.0")
    assert_equal "true", resolve("false != true")
    assert_equal "false", resolve("1em == 1px")
    assert_equal "false", resolve("12 != 12")
    assert_equal "true", resolve("(foo bar baz) == (foo bar baz)")
    assert_equal "true", resolve("(foo, bar, baz) == (foo, bar, baz)")
    assert_equal "true", resolve('((1 2), (3, 4), (5 6)) == ((1 2), (3, 4), (5 6))')
    assert_equal "true", resolve('((1 2), (3 4)) == (1 2, 3 4)')
    assert_equal "false", resolve('((1 2) 3) == (1 2 3)')
    assert_equal "false", resolve('(1 (2 3)) == (1 2 3)')
    assert_equal "false", resolve('((1, 2) (3, 4)) == (1, 2 3, 4)')
    assert_equal "false", resolve('(1 2 3) == (1, 2, 3)')

    assert_equal "true", resolve('null == null')
    assert_equal "false", resolve('"null" == null')
    assert_equal "false", resolve('0 == null')
    assert_equal "false", resolve('() == null')

    assert_equal "false", resolve('null != null')
    assert_equal "true", resolve('"null" != null')
    assert_equal "true", resolve('0 != null')
    assert_equal "true", resolve('() != null')
  end

  def test_mod
    assert_equal "5", resolve("29 % 12")
    assert_equal "5px", resolve("29px % 12")
    assert_equal "5px", resolve("29px % 12px")
  end

  def test_operation_precedence
    assert_equal "false true", resolve("true and false false or true")
    assert_equal "true", resolve("false and true or true and true")
    assert_equal "true", resolve("1 == 2 or 3 == 3")
    assert_equal "true", resolve("1 < 2 == 3 >= 3")
    assert_equal "true", resolve("1 + 3 > 4 - 2")
    assert_equal "11", resolve("1 + 2 * 3 + 4")
  end

  def test_functions
    assert_equal "#80ff80", resolve("hsl(120, 100%, 75%)")
    assert_equal "#81ff81", resolve("hsl(120, 100%, 75%) + #010001")
  end

  def test_operator_unit_conversion
    assert_equal "1.1cm", resolve("1cm + 1mm")
    assert_equal "2in", resolve("1in + 96px")
    assert_equal "true", resolve("2mm < 1cm")
    assert_equal "true", resolve("10mm == 1cm")
    assert_equal "true", resolve("1.1cm == 11mm")

    assert_warning(<<WARNING) {assert_equal "true", resolve("1 == 1cm")}
DEPRECATION WARNING on line 1 of test_operator_unit_conversion_inline.sass:
The result of `1 == 1cm` will be `false` in future releases of Sass.
Unitless numbers will no longer be equal to the same numbers with units.
WARNING

    assert_warning(<<WARNING) {assert_equal "false", resolve("1 != 1cm")}
DEPRECATION WARNING on line 1 of test_operator_unit_conversion_inline.sass:
The result of `1 != 1cm` will be `true` in future releases of Sass.
Unitless numbers will no longer be equal to the same numbers with units.
WARNING
  end

  def test_length_units
    assert_equal "2.54", resolve("(1in/1cm)")
    assert_equal "2.3622", resolve("(1cm/1pc)")
    assert_equal "4.23333", resolve("(1pc/1mm)")
    assert_equal "2.83465", resolve("(1mm/1pt)")
    assert_equal "1.33333", resolve("(1pt/1px)")
    assert_equal "0.01042", resolve("(1px/1in)")
  end

  def test_angle_units
    assert_equal "1.11111", resolve("(1deg/1grad)")
    assert_equal "0.01571", resolve("(1grad/1rad)")
    assert_equal "0.15915", resolve("(1rad/1turn)")
    assert_equal "360", resolve("(1turn/1deg)")
  end

  def test_time_units
    assert_equal "1000", resolve("(1s/1ms)")
  end

  def test_frequency_units
    assert_equal "0.001", resolve("(1Hz/1kHz)")
  end

  def test_resolution_units
    assert_equal "2.54", resolve("(1dpi/1dpcm)")
    assert_equal "37.79528", resolve("(1dpcm/1dppx)")
    assert_equal "0.01042", resolve("(1dppx/1dpi)")
  end

  def test_operations_have_options
    assert_equal "Options defined!", resolve("assert_options(1 + 1)")
    assert_equal "Options defined!", resolve("assert_options('bar' + 'baz')")
  end

  def test_slash_compiles_literally_when_left_alone
    assert_equal "1px/2px", resolve("1px/2px")
    assert_equal "1px/2px/3px/4px", resolve("1px/2px/3px/4px")

    assert_equal "1px/2px redpx bluepx", resolve("1px/2px redpx bluepx")
    assert_equal "foo 1px/2px/3px bar", resolve("foo 1px/2px/3px bar")
  end

  def test_slash_divides_with_parens
    assert_equal "0.5", resolve("(1px/2px)")
    assert_equal "0.5", resolve("(1px)/2px")
    assert_equal "0.5", resolve("1px/(2px)")
  end

  def test_slash_divides_with_other_arithmetic
    assert_equal "0.5px", resolve("1px*1px/2px")
    assert_equal "0.5px", resolve("1px/2px*1px")
    assert_equal "0.5", resolve("0+1px/2px")
    assert_equal "0.5", resolve("1px/2px+0")
  end

  def test_slash_divides_with_variable
    assert_equal "0.5", resolve("$var/2px", {}, env("var" => eval("1px")))
    assert_equal "0.5", resolve("1px/$var", {}, env("var" => eval("2px")))
    assert_equal "0.5", resolve("$var", {}, env("var" => eval("1px/2px")))
  end

  def test_non_ident_colors_with_wrong_number_of_digits
    assert_raise_message(Sass::SyntaxError,
      'Invalid CSS after "": expected expression (e.g. 1px, bold), was "#1"') {eval("#1")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid CSS after "": expected expression (e.g. 1px, bold), was "#12"') {eval("#12")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid CSS after "": expected expression (e.g. 1px, bold), was "#1234"') {eval("#1234")}
    assert_raise_message(Sass::SyntaxError,
      'Invalid CSS after "": expected expression (e.g. 1px, bold), was "#12345"') {eval("#12345")}
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "": expected expression (e.g. ' \
      '1px, bold), was "#1234567"') {eval("#1234567")}
  end

  def test_case_insensitive_color_names
    assert_equal "BLUE", resolve("BLUE")
    assert_equal "rEd", resolve("rEd")
    assert_equal "#804000", resolve("mix(GrEeN, ReD)")
  end

  def test_empty_list
    assert_equal "1 2 3", resolve("1 2 () 3")
    assert_equal "1 2 3", resolve("1 2 3 ()")
    assert_equal "1 2 3", resolve("() 1 2 3")
    assert_raise_message(Sass::SyntaxError, "() isn't a valid CSS value.") {resolve("()")}
    assert_raise_message(Sass::SyntaxError, "() isn't a valid CSS value.") {resolve("nth(append((), ()), 1)")}
  end

  def test_list_with_nulls
    assert_equal "1, 2, 3", resolve("1, 2, null, 3")
    assert_equal "1 2 3", resolve("1 2 null 3")
    assert_equal "1, 2, 3", resolve("1, 2, 3, null")
    assert_equal "1 2 3", resolve("1 2 3 null")
    assert_equal "1, 2, 3", resolve("null, 1, 2, 3")
    assert_equal "1 2 3", resolve("null 1 2 3")
  end

  def test_map_can_have_trailing_comma
    assert_equal("(foo: 1, bar: 2)", eval("(foo: 1, bar: 2,)").to_sass)
  end

  def test_list_can_have_trailing_comma
    assert_equal("1, 2, 3", resolve("1, 2, 3,"))
  end

  def test_trailing_comma_defines_singleton_list
    assert_equal("1 2 3", resolve("nth((1 2 3,), 1)"))
  end

  def test_map_cannot_have_duplicate_keys
    assert_raise_message(Sass::SyntaxError, 'Duplicate key "foo" in map (foo: bar, foo: baz).') do
      eval("(foo: bar, foo: baz)")
    end
    assert_raise_message(Sass::SyntaxError, 'Duplicate key "foo" in map (foo: bar, fo + o: baz).') do
      eval("(foo: bar, fo + o: baz)")
    end
    assert_raise_message(Sass::SyntaxError, 'Duplicate key "foo" in map (foo: bar, "foo": baz).') do
      eval("(foo: bar, 'foo': baz)")
    end
    assert_raise_message(Sass::SyntaxError, 'Duplicate key 2px in map (2px: bar, 1px + 1px: baz).') do
      eval("(2px: bar, 1px + 1px: baz)")
    end
    assert_raise_message(Sass::SyntaxError, 'Duplicate key #0000ff in map (blue: bar, #00f: baz).') do
      eval("(blue: bar, #00f: baz)")
    end
  end

  def test_non_duplicate_map_keys
    # These shouldn't throw errors
    eval("(foo: foo, bar: bar)")
    eval("(2px: foo, 2: bar)")
    eval("(2px: foo, 2em: bar)")
    eval("('2px': foo, 2px: bar)")
  end

  def test_map_syntax_errors
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "(foo:": expected expression (e.g. 1px, bold), was ")"') do
      eval("(foo:)")
    end
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "(": expected ")", was ":bar)"') do
      eval("(:bar)")
    end
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "(foo, bar": expected ")", was ": baz)"') do
      eval("(foo, bar: baz)")
    end
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "(foo: bar, baz": expected ":", was ")"') do
      eval("(foo: bar, baz)")
    end
  end

  def test_deep_argument_error_not_unwrapped
    # JRuby (as of 1.6.7.2) offers no way of distinguishing between
    # argument errors caused by programming errors in a function and
    # argument errors explicitly thrown within that function.
    return if RUBY_PLATFORM =~ /java/

    # Don't validate the message; it's different on Rubinius.
    assert_raises(ArgumentError) {resolve("arg-error()")}
  end

  def test_shallow_argument_error_unwrapped
    assert_raise_message(Sass::SyntaxError, "wrong number of arguments (1 for 0) for `arg-error'") {resolve("arg-error(1)")}
  end

  def test_boolean_ops_short_circuit
    assert_equal "false", resolve("$ie and $ie <= 7", {}, env('ie' => Sass::Script::Value::Bool.new(false)))
    assert_equal "true", resolve("$ie or $undef", {}, env('ie' => Sass::Script::Value::Bool.new(true)))
  end

  def test_selector
    env = Sass::Environment.new
    assert_equal "true", resolve("& == null", {}, env)

    env.selector = selector('.foo.bar .baz.bang, .bip.bop')
    assert_equal ".foo.bar .baz.bang, .bip.bop", resolve("&", {}, env)
    assert_equal ".foo.bar .baz.bang", resolve("nth(&, 1)", {}, env)
    assert_equal ".bip.bop", resolve("nth(&, 2)", {}, env)
    assert_equal ".foo.bar", resolve("nth(nth(&, 1), 1)", {}, env)
    assert_equal ".baz.bang", resolve("nth(nth(&, 1), 2)", {}, env)
    assert_equal ".bip.bop", resolve("nth(nth(&, 2), 1)", {}, env)
    assert_equal "string", resolve("type-of(nth(nth(&, 1), 1))", {}, env)

    env.selector = selector('.foo > .bar')
    assert_equal ".foo > .bar", resolve("&", {}, env)
    assert_equal ".foo > .bar", resolve("nth(&, 1)", {}, env)
    assert_equal ".foo", resolve("nth(nth(&, 1), 1)", {}, env)
    assert_equal ">", resolve("nth(nth(&, 1), 2)", {}, env)
    assert_equal ".bar", resolve("nth(nth(&, 1), 3)", {}, env)
  end

  def test_selector_with_newlines
    env = Sass::Environment.new
    env.selector = selector(".foo.bar\n.baz.bang,\n\n.bip.bop")
    assert_equal ".foo.bar .baz.bang, .bip.bop", resolve("&", {}, env)
    assert_equal ".foo.bar .baz.bang", resolve("nth(&, 1)", {}, env)
    assert_equal ".bip.bop", resolve("nth(&, 2)", {}, env)
    assert_equal ".foo.bar", resolve("nth(nth(&, 1), 1)", {}, env)
    assert_equal ".baz.bang", resolve("nth(nth(&, 1), 2)", {}, env)
    assert_equal ".bip.bop", resolve("nth(nth(&, 2), 1)", {}, env)
    assert_equal "string", resolve("type-of(nth(nth(&, 1), 1))", {}, env)
  end

  def test_setting_global_variable_globally
    assert_no_warning {assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))}
.foo {
  a: 1; }

.bar {
  b: 2; }
CSS
$var: 1;

.foo {
  a: $var;
}

$var: 2;

.bar {
  b: $var;
}
SCSS
  end

  def test_setting_global_variable_locally
    assert_no_warning {assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))}
.bar {
  a: x;
  b: y;
  c: z; }
CSS
$var1: 1;
$var3: 3;

.foo {
  $var1: x !global;
  $var2: y !global;
  @each $var3 in _ {
    $var3: z !global;
  }
}

.bar {
  a: $var1;
  b: $var2;
  c: $var3;
}
SCSS
  end

  def test_setting_global_variable_locally_with_default
    assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))
.bar {
  a: 1;
  b: y;
  c: z; }
CSS
$var1: 1;

.foo {
  $var1: x !global !default;
  $var2: y !global !default;
  @each $var3 in _ {
    $var3: z !global !default;
  }
}

.bar {
  a: $var1;
  b: $var2;
  c: $var3;
}
SCSS
  end

  def test_setting_local_variable
    assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))
.a {
  value: inside; }

.b {
  value: outside; }
CSS
$var: outside;

.a {
  $var: inside;
  value: $var;
}

.b {
  value: $var;
}
SCSS
  end

  def test_setting_local_variable_from_inner_scope
    assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))
.a .b {
  value: inside; }
.a .c {
  value: inside; }
CSS
.a {
  $var: outside;

  .b {
    $var: inside;
    value: $var;
  }

  .c {
    value: $var;
  }
}
SCSS
  end

  def test_if_can_assign_to_global_variables
    assert_equal <<CSS, render(<<SCSS, :syntax => :scss)
.a {
  b: 2; }
CSS
$var: 1;
@if true {$var: 2}
.a {b: $var}
SCSS
  end

  def test_else_can_assign_to_global_variables
    assert_equal <<CSS, render(<<SCSS, :syntax => :scss)
.a {
  b: 2; }
CSS
$var: 1;
@if false {}
@else {$var: 2}
.a {b: $var}
SCSS
  end

  def test_for_can_assign_to_global_variables
    assert_equal <<CSS, render(<<SCSS, :syntax => :scss)
.a {
  b: 2; }
CSS
$var: 1;
@for $i from 1 to 2 {$var: 2}
.a {b: $var}
SCSS
  end

  def test_each_can_assign_to_global_variables
    assert_equal <<CSS, render(<<SCSS, :syntax => :scss)
.a {
  b: 2; }
CSS
$var: 1;
@each $a in 1 {$var: 2}
.a {b: $var}
SCSS
  end

  def test_while_can_assign_to_global_variables
    assert_equal <<CSS, render(<<SCSS, :syntax => :scss)
.a {
  b: 2; }
CSS
$var: 1;
@while $var != 2 {$var: 2}
.a {b: $var}
SCSS
  end

  def test_if_doesnt_leak_local_variables
    assert_raise_message(Sass::SyntaxError, 'Undefined variable: "$var".') do
      render(<<SCSS, :syntax => :scss)
@if true {$var: 1}
.a {b: $var}
SCSS
    end
  end

  def test_else_doesnt_leak_local_variables
    assert_raise_message(Sass::SyntaxError, 'Undefined variable: "$var".') do
      render(<<SCSS, :syntax => :scss)
@if false {}
@else {$var: 1}
.a {b: $var}
SCSS
    end
  end

  def test_for_doesnt_leak_local_variables
    assert_raise_message(Sass::SyntaxError, 'Undefined variable: "$var".') do
      render(<<SCSS, :syntax => :scss)
@for $i from 1 to 2 {$var: 1}
.a {b: $var}
SCSS
    end
  end

  def test_each_doesnt_leak_local_variables
    assert_raise_message(Sass::SyntaxError, 'Undefined variable: "$var".') do
      render(<<SCSS, :syntax => :scss)
@each $a in 1 {$var: 1}
.a {b: $var}
SCSS
    end
  end

  def test_while_doesnt_leak_local_variables
    assert_raise_message(Sass::SyntaxError, 'Undefined variable: "$var".') do
      render(<<SCSS, :syntax => :scss)
$iter: true;
@while $iter {
  $var: 1;
  $iter: false;
}
.a {b: $var}
SCSS
    end
  end

  def test_color_format_is_preserved_by_default
    assert_equal "blue", resolve("blue")
    assert_equal "bLuE", resolve("bLuE")
    assert_equal "#00f", resolve("#00f")
    assert_equal "blue #00F", resolve("blue #00F")
    assert_equal "blue", resolve("nth(blue #00F, 1)")
    assert_equal "#00F", resolve("nth(blue #00F, 2)")
  end

  def test_color_format_isnt_always_preserved_in_compressed_style
    assert_equal "red", resolve("red", :style => :compressed)
    assert_equal "red", resolve("#f00", :style => :compressed)
    assert_equal "red red", resolve("red #f00", :style => :compressed)
    assert_equal "red", resolve("nth(red #f00, 2)", :style => :compressed)
  end

  def test_color_format_is_sometimes_preserved_in_compressed_style
    assert_equal "ReD", resolve("ReD", :style => :compressed)
    assert_equal "blue", resolve("blue", :style => :compressed)
    assert_equal "#00f", resolve("#00f", :style => :compressed)
  end

  def test_color_format_isnt_preserved_when_modified
    assert_equal "magenta", resolve("#f00 + #00f")
  end

  def test_ids
    assert_equal "#foo", resolve("#foo")
    assert_equal "#abcd", resolve("#abcd")
    assert_equal "#abc-def", resolve("#abc-def")
    assert_equal "#abc_def", resolve("#abc_def")
    assert_equal "#uvw-xyz", resolve("#uvw-xyz")
    assert_equal "#uvw_xyz", resolve("#uvw_xyz")
    assert_equal "#uvwxyz", resolve("#uvw + xyz")
  end

  def test_scientific_notation
    assert_equal "2000", resolve("2e3")
    assert_equal "2000", resolve("2E3")
    assert_equal "2000", resolve("2e+3")
    assert_equal "2000em", resolve("2e3em")
    assert_equal "25000000000", resolve("2.5e10")
    assert_equal "0.1234", resolve("1234e-4")
    assert_equal "12.34", resolve("1.234e1")
  end

  def test_identifier_units
    assert_equal "5-foo", resolve("2-foo + 3-foo")
    assert_equal "5-foo-", resolve("2-foo- + 3-foo-")
    assert_equal "5-\\u2603", resolve("2-\\u2603 + 3-\\u2603")
  end

  def test_backslash_newline_in_string
    assert_equal 'foobar', resolve("\"foo\\\nbar\"")
    assert_equal 'foobar', resolve("'foo\\\nbar'")
  end

  def test_unclosed_special_fun
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "calc(foo()": expected ")", was ""') do
      resolve("calc(foo()")
    end
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "calc(#{\')\'}": expected ")", was ""') do
      resolve("calc(\#{')'}")
    end
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "calc(#{foo": expected "}", was ""') do
      resolve("calc(\#{foo")
    end
  end

  def test_special_fun_with_interpolation
    assert_equal "calc())", resolve("calc(\#{')'})")
    assert_equal "calc(# {foo})", resolve("calc(# {foo})")
  end

  # Regression Tests

  def test_interpolation_after_string
    assert_equal '"foobar" 2', resolve('"foobar" #{2}')
    assert_equal "calc(1 + 2) 3", resolve('calc(1 + 2) #{3}')
  end

  def test_repeatedly_modified_color
    assert_equal(<<CSS, render(<<SASS))
a {
  link-color: #161C14;
  link-color-hover: black;
  link-color-tap: rgba(22, 28, 20, 0.3); }
CSS
$green: #161C14
$link-color: $green
$link-color-hover: darken($link-color, 10%)
$link-color-tap: rgba($green, 0.3)

a
  link-color: $link-color
  link-color-hover: $link-color-hover
  link-color-tap: $link-color-tap
SASS
  end

  def test_inspect_divided_numbers
    assert_equal "1px/2px", resolve("inspect(1px/2px)")
    assert_equal "0.5", resolve("inspect((1px/2px))")
  end

  def test_minus_without_whitespace
    assert_equal "5px", resolve("15px-10px")
    assert_equal "5px-", resolve("15px--10px-")
  end

  def test_minus_preceded_by_comment
    assert_equal "15px -10px", resolve("15px/**/-10px")
  end

  def test_user_defined_function_forces_division
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 10px; }
CSS
@function foo()
  @return 20px

a
  b: (foo() / 2)
SASS

    assert_equal(<<CSS, render(<<SASS))
a {
  b: 10px; }
CSS
@function foo()
  @return 20px

a
  b: foo() / 2
SASS
end

  def test_funcall_has_higher_precedence_than_color_name
    assert_equal "teal(12)", resolve("teal(12)")
    assert_equal "tealbang(12)", resolve("tealbang(12)")
    assert_equal "teal-bang(12)", resolve("teal-bang(12)")
    assert_equal "teal\\+bang(12)", resolve("teal\\+bang(12)")
  end

  def test_funcall_has_higher_precedence_than_true_false_null
    assert_equal "teal(12)", resolve("teal(12)")
    assert_equal "tealbang(12)", resolve("tealbang(12)")
    assert_equal "teal-bang(12)", resolve("teal-bang(12)")
    assert_equal "teal\\+bang(12)", resolve("teal\\+bang(12)")
  end

  def test_and_or_not_disallowed_as_function_names
    %w[and or not].each do |name|
      assert_raise_message(Sass::SyntaxError, "Invalid function name \"#{name}\".") do
        render(<<SASS)
@function #{name}()
  @return null
SASS
      end
    end
  end

  def test_interpolation_after_hash
    assert_equal "#2", resolve('"##{1 + 1}"')
  end

  def test_misplaced_comma_in_funcall
    assert_raise_message(Sass::SyntaxError,
      'Invalid CSS after "foo(bar, ": expected function argument, was ")"') {eval('foo(bar, )')}
  end

  def test_color_prefixed_identifier
    assert_equal "tealbang", resolve("tealbang")
    assert_equal "teal-bang", resolve("teal-bang")
  end

  def test_op_prefixed_identifier
    assert_equal "notbang", resolve("notbang")
    assert_equal "not-bang", resolve("not-bang")
    assert_equal "or-bang", resolve("or-bang")
    assert_equal "and-bang", resolve("and-bang")
  end

  def test_number_initialization
    assert_equal Sass::Script::Value::Number.new(10, ["px"]), Sass::Script::Value::Number.new(10, "px")
    assert_equal Sass::Script::Value::Number.new(10, ["px"], ["em"]), Sass::Script::Value::Number.new(10, "px", "em")
  end

  def test_is_unit
    assert Sass::Script::Value::Number.new(10, "px").is_unit?("px")
    assert Sass::Script::Value::Number.new(10).is_unit?(nil)
    assert !Sass::Script::Value::Number.new(10, "px", "em").is_unit?("px")
    assert !Sass::Script::Value::Number.new(10, [], "em").is_unit?("em")
    assert !Sass::Script::Value::Number.new(10, ["px", "em"]).is_unit?("px")
  end

  def test_rename_redirect
    assert_no_warning do
      assert_equal Sass::Script::Value::Base, Sass::Script::Literal
      assert_equal Sass::Script::Tree::Node, Sass::Script::Node
      assert_equal Sass::Script::Tree::Operation, Sass::Script::Operation
      assert_equal Sass::Script::Value::String, Sass::Script::String
    end
  end

  def test_number_printing
    assert_equal "1", resolve("1")
    assert_equal "1", resolve("1.0")
    assert_equal "1000000000", resolve("1000000000")
    assert_equal "0.00001", resolve("0.00001")
    assert_equal "1.12121", resolve("1.121214")
    assert_equal "1.12122", resolve("1.121215")
    assert_equal "Infinity", resolve("(1.0/0.0)")
    assert_equal "-Infinity", resolve("(-1.0/0.0)")
    assert_equal "NaN", resolve("(0.0/0.0)")
  end

  private

  def resolve(str, opts = {}, environment = env)
    munge_filename opts
    val = eval(str, opts, environment)
    assert_kind_of Sass::Script::Value::Base, val
    val.is_a?(Sass::Script::Value::String) ? val.value : val.to_s
  end

  def resolve_quoted(str, opts = {}, environment = env)
    munge_filename opts
    val = eval(str, opts, environment)
    assert_kind_of Sass::Script::Value::Base, val
    val.to_s
  end

  def assert_unquoted(str, opts = {}, environment = env)
    munge_filename opts
    val = eval(str, opts, environment)
    assert_kind_of Sass::Script::Value::String, val
    assert_equal :identifier, val.type
  end

  def assert_quoted(str, opts = {}, environment = env)
    munge_filename opts
    val = eval(str, opts, environment)
    assert_kind_of Sass::Script::Value::String, val
    assert_equal :string, val.type
  end

  def eval(str, opts = {}, environment = env)
    munge_filename opts
    Sass::Script.parse(str, opts.delete(:line) || 1,
      opts.delete(:offset) || 0, opts).
      perform(Sass::Environment.new(environment, opts))
  end

  def render(sass, options = {})
    munge_filename options
    Sass::Engine.new(sass, options).render
  end

  def env(hash = {})
    env = Sass::Environment.new
    hash.each {|k, v| env.set_var(k, v)}
    env
  end

  def selector(str)
    parser = Sass::SCSS::StaticParser.new(
      str, filename_for_test, Sass::Importers::Filesystem.new('.'))
    parser.parse_selector
  end

  def test_null_is_a_singleton
    assert_same Sass::Script::Value::Null.new, Sass::Script::Value::Null.new
  end
end
