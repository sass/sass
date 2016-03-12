#!/usr/bin/env ruby

require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class CssVariableTest < MiniTest::Test
  def test_variable_warning_for_operators
    resolve_with_variable_warning("1 == 2")
    resolve_with_variable_warning("1 != 2")
    resolve_with_variable_warning("1 < 2")
    resolve_with_variable_warning("1 <= 2")
    resolve_with_variable_warning("1 > 2")
    resolve_with_variable_warning("1 >= 2")
    resolve_with_variable_warning("1 + 1")
    resolve_with_variable_warning("1 - 1")
    resolve_with_variable_warning("1 * 1")
    resolve_with_variable_warning("1 % 1")
  end

  def test_variable_warning_for_variable
    render_with_variable_warning(<<SCSS, "$var", 3)
.foo {
  $var: value;
  --var: $var;
}
SCSS
  end

  def test_variable_warning_for_core_function
    resolve_with_variable_warning("alpha(#abc)")
  end

  def test_variable_warning_for_sass_function
    render_with_variable_warning(<<SCSS, "my-fn()", 2)
@function my-fn() {@return null}
.foo {--var: my-fn()}
SCSS
  end

  def test_variable_warning_for_parens
    resolve_with_variable_warning("(foo)", "foo")
    resolve_with_variable_warning("(foo,)")
  end

  def test_variable_warning_for_selector
    resolve_with_variable_warning("&")
  end

  def test_variable_warning_for_nested_properties
    assert_warning(<<WARNING) {render(<<SCSS)}
DEPRECATION WARNING on line 2 of #{filename_for_test :scss}:
Sass 3.6 will change the way CSS variables are parsed. Instead of being parsed as
normal properties, they will not allow any Sass-specific behavior other than \#{}.
WARNING
.foo {
  --var: {
    a: b;
  }
}
SCSS
  end

  def test_no_warning
    assert_no_variable_warning("foo")
    assert_no_variable_warning("true")
    assert_no_variable_warning("1, 2")
    assert_no_variable_warning("1 2")
    assert_no_variable_warning("1 / 2")
    assert_no_variable_warning("foo / bar")
    assert_no_variable_warning("asdf(foo)")
    assert_no_variable_warning("calc(1 + 1)")
    assert_no_variable_warning("asdf(foo=2)")
  end

  def test_no_warning_within_interpolation
    assert_no_variable_warning('#{1 + 1}')
    assert_no_variable_warning('#{alpha(#abc)}')
  end

  private

  def assert_no_variable_warning(str)
    assert_no_warning {render("a {--var: #{str}}")}
    assert_no_warning {render("a\n  --var: #{str}", :syntax => :sass)}
  end

  def resolve_with_variable_warning(str, expression = nil)
    render_with_variable_warning("a {--var: #{str}}", expression || str, 1)
    render_with_variable_warning(
      "a\n  --var: #{str}", expression || str, 2, :syntax => :sass)
  end

  def render_with_variable_warning(sass, expression, line, opts = {})
    opts[:syntax] ||= :scss
    assert_warning(<<WARNING) {render(sass, opts)}
DEPRECATION WARNING on line #{line} of #{filename_for_test(opts[:syntax])}:
Sass 3.6 will change the way CSS variables are parsed. Instead of being parsed as
normal properties, they will not allow any Sass-specific behavior other than \#{}.
For forwards-compatibility, use \#{}:

  --variable: \#{#{expression}};
WARNING
  end

  def render(sass, options = {})
    options[:syntax] ||= :scss
    options[:cache] = false
    munge_filename options
    Sass::Engine.new(sass, options).render
  end

  def resolve(str, opts = {}, environment = env)
    munge_filename opts
    val = eval(str, opts, environment)
    assert_kind_of Sass::Script::Value::Base, val
    val.options = opts
    val.is_a?(Sass::Script::Value::String) ? val.value : val.to_s
  end

  def eval(str, opts = {}, environment = env)
    munge_filename opts
    Sass::Script.parse(str, opts.delete(:line) || 1,
      opts.delete(:offset) || 0, opts).
      perform(Sass::Environment.new(environment, opts))
  end

  def env(hash = {})
    env = Sass::Environment.new
    hash.each {|k, v| env.set_var(k, v)}
    env
  end
end
