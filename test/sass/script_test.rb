#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class SassScriptTest < Test::Unit::TestCase
  include Sass::Script

  def test_color_checks_input
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([1, 2, -1])}
    assert_raise(Sass::SyntaxError, "Color values must be between 0 and 255") {Color.new([256, 2, 3])}
  end

  def test_string_escapes
    assert_equal '"', resolve("\"\\\"\"")
    assert_equal "\\", resolve("\"\\\\\"")
    assert_equal "\\02fa", resolve("\"\\02fa\"")
  end

  def test_color_names
    assert_equal "white", resolve("white")
    assert_equal "white", resolve("#ffffff")
    assert_equal "#fffffe", resolve("white - #000001")
  end

  def test_implicit_strings
    silence_warnings do
      assert_equal Sass::Script::String.new("foo"), eval("foo")
      assert_equal Sass::Script::String.new("foo bar"), eval("foo bar")
      assert_equal Sass::Script::String.new("foo/bar"), eval("foo/bar")
    end
  end

  def test_interpolation
    assert_equal "foo bar, baz bang", resolve('"foo #{"bar"}, #{"baz"} bang"')
    assert_equal "foo bar baz bang", resolve('"foo #{"#{"ba" + "r"} baz"} bang"')
    assert_equal 'foo #{bar baz} bang', resolve('"foo \#{#{"ba" + "r"} baz} bang"')
    assert_equal 'foo #{baz bang', resolve('"foo #{"\#{" + "baz"} bang"')
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
foo \#{bar baz} bang {
  a: b; }
CSS
foo \\\#{\#{"ba" + "r"} baz} bang
  a: b
SASS
    assert_equal(<<CSS, render(<<SASS))
foo \#{baz bang {
  a: b; }
CSS
foo \#{"\\\#{" + "baz"} bang
  a: b
SASS
  end

  def test_implicit_string_warning
    assert_warning(<<WARN) {eval("foo")}
DEPRECATION WARNING:
On line 1, character 1 of 'test_implicit_string_warning_inline.sass'
Implicit strings have been deprecated and will be removed in version 2.4.
'foo' was not quoted. Please add double quotes (e.g. "foo").
WARN
    assert_warning(<<WARN) {eval("1 + foo")}
DEPRECATION WARNING:
On line 1, character 5 of 'test_implicit_string_warning_inline.sass'
Implicit strings have been deprecated and will be removed in version 2.4.
'foo' was not quoted. Please add double quotes (e.g. "foo").
WARN
    assert_warning(<<WARN) {render("@if 1 + foo")}
DEPRECATION WARNING:
On line 1, character 9 of 'test_implicit_string_warning_inline.sass'
Implicit strings have been deprecated and will be removed in version 2.4.
'foo' was not quoted. Please add double quotes (e.g. "foo").
WARN

    # Regression
    assert_warning(<<WARN) {render("@if if")}
DEPRECATION WARNING:
On line 1, character 5 of 'test_implicit_string_warning_inline.sass'
Implicit strings have been deprecated and will be removed in version 2.4.
'if' was not quoted. Please add double quotes (e.g. "if").
WARN
  end

  def test_inaccessible_functions
    assert_warning <<WARN do
DEPRECATION WARNING:
On line 2, character 6 of 'test_inaccessible_functions_inline.sass'
Implicit strings have been deprecated and will be removed in version 2.4.
'to_s' was not quoted. Please add double quotes (e.g. "to_s").
WARN
      assert_equal "send(to_s)", resolve("send(to_s)", :line => 2)
    end
    assert_equal "public_instance_methods()", resolve("public_instance_methods()")
  end

  def test_hyphen_warning
    a = Sass::Script::String.new("a")
    b = Sass::Script::String.new("b")
    assert_warning(<<WARN) {eval("!a-!b", {}, env("a" => a, "b" => b))}
DEPRECATION WARNING:
On line 1, character 3 of 'test_hyphen_warning_inline.sass'
- will be allowed as part of variable names in version 2.4.
Please add whitespace to separate it from the previous token.
WARN

    assert_warning(<<WARN) {eval("true-false")}
DEPRECATION WARNING:
On line 1, character 5 of 'test_hyphen_warning_inline.sass'
- will be allowed as part of variable names in version 2.4.
Please add whitespace to separate it from the previous token.
WARN
  end

  def test_ruby_equality
    assert_equal eval('"foo"'), eval('"foo"')
    assert_equal eval('1'), eval('1.0')
    assert_not_equal eval('1'), eval('"1"')
  end

  def test_booleans
    assert_equal "true", resolve("true")
    assert_equal "false", resolve("false")
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
  end

  def test_arithmetic_ops
    assert_equal "2", resolve("1 + 1")
    assert_equal "0", resolve("1 - 1")
    assert_equal "8", resolve("2 * 4")
    assert_equal "0.5", resolve("2 / 4")
    assert_equal "2", resolve("4 / 2")

    assert_equal "-1", resolve("-1")
  end

  def test_string_ops
    assert_equal "foo bar", resolve('"foo" "bar"')
    assert_equal "true 1", resolve('true 1')
    assert_equal "foo, bar", resolve('"foo" , "bar"')
    assert_equal "true, 1", resolve('true , 1')
    assert_equal "foobar", resolve('"foo" + "bar"')
    assert_equal "true1", resolve('true + 1')
    assert_equal "foo-bar", resolve('"foo" - "bar"')
    assert_equal "true-1", resolve('true - 1')
    assert_equal "foo/bar", resolve('"foo" / "bar"')
    assert_equal "true/1", resolve('true / 1')

    assert_equal "-bar", resolve('- "bar"')
    assert_equal "-true", resolve('- true')
    assert_equal "/bar", resolve('/ "bar"')
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

  def test_equals
    assert_equal("true", resolve('"foo" == !foo', {},
        env("foo" => Sass::Script::String.new("foo"))))
    assert_equal "true", resolve("1 == 1.0")
    assert_equal "true", resolve("false != true")
    assert_equal "false", resolve("1em == 1px")
    assert_equal "false", resolve("12 != 12")
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
    assert_equal "true", resolve("2mm < 1cm")
    assert_equal "true", resolve("10mm == 1cm")
    assert_equal "true", resolve("1 == 1cm")
    assert_equal "true", resolve("1.1cm == 11mm")
  end

  private

  def resolve(str, opts = {}, environment = env)
    munge_filename opts
    eval(str, opts, environment).to_s
  end

  def eval(str, opts = {}, environment = env)
    munge_filename opts
    Sass::Script.parse(str, opts[:line] || 1,
      opts[:offset] || 0, opts[:filename]).perform(environment)
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

  def test_number_printing
    assert_equal "1", eval("1")
    assert_equal "1", eval("1.0")
    assert_equal "1.121", eval("1.1214")
    assert_equal "1.122", eval("1.1215")
    assert_equal "Infinity", eval("1.0/0.0")
    assert_equal "-Infinity", eval("-1.0/0.0")
    assert_equal "NaN", eval("0.0/0.0")
  end
end
