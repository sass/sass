#!/usr/bin/env ruby

require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class CssVariableTest < MiniTest::Test
  # Conversion.

  def test_static_values_convert
    assert_converts <<SASS, <<SCSS
.foo
  --bar: baz
SASS
.foo {
  --bar: baz;
}
SCSS

    assert_converts <<SASS, <<SCSS
.foo
  --bar: [({{([!;])}})]
SASS
.foo {
  --bar: [({{([!;])}})];
}
SCSS

    assert_converts <<SASS, <<SCSS
.foo
  --bar: {a: b; c: d}
SASS
.foo {
  --bar: {a: b; c: d};
}
SCSS
  end

  def test_dynamic_values_convert
    assert_converts <<SASS, <<SCSS
.foo
  --bar: baz \#{bang} qux
SASS
.foo {
  --bar: baz \#{bang} qux;
}
SCSS

    assert_converts <<SASS, <<SCSS
.foo
  --bar: "baz \#{bang} qux"
SASS
.foo {
  --bar: "baz \#{bang} qux";
}
SCSS
  end

  def test_multiline_value_converts
    assert_scss_to_scss <<SCSS
.foo {
  --bar: {
    a: b;
    c: d;
  };
}
SCSS

    assert_scss_to_sass <<SASS, <<SCSS
.foo
  --bar: {     a: b;     c: d;   }
SASS
.foo {
  --bar: {
    a: b;
    c: d;
  };
}
SCSS
  end

  private

  def assert_variable_value(expected, source)
    expected = <<CSS
x {
  --variable: #{expected}; }
CSS

    assert_equal expected, render_variable(source)
    assert_equal expected, render_variable(source, syntax: :sass)
  end

  def render_variable(source, syntax: :scss)
    render(syntax == :scss ? <<SCSS : <<SASS, :syntax => syntax)
x {
  --variable: #{source};
}
SCSS
x
  --variable: #{source}
SASS
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
