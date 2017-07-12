
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

# Most CSS variable tests are in sass-spec, but a few relate to formatting or
# conversion and so belong here.
class CssVariableTest < MiniTest::Test
  def test_folded_inline_whitespace
    assert_variable_value "foo bar baz", "foo    bar        baz"
    assert_variable_value "foo bar", "foo \t   bar"
  end

  def test_folded_multiline_whitespace
    # We don't want to reformat newlines in nested and expanded mode, so we just
    # remove trailing whitespace before them.
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --multiline: foo
    bar; }
CSS
.foo {
  --multiline: foo\s
    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  --multiline: foo


    bar; }
CSS
.foo {
  --multiline: foo\s


    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS, style: :expanded)
.foo {
  --multiline: foo
    bar;
}
CSS
.foo {
  --multiline: foo\s
    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS, style: :expanded)
.foo {
  --multiline: foo


    bar;
}
CSS
.foo {
  --multiline: foo\s


    bar;
}
SCSS

    # In compact and compressed mode, we fold all whitespace around newlines
    # together.
    assert_equal <<CSS, render(<<SCSS, style: :compact)
.foo { --multiline: foo bar; }
CSS
.foo {
  --multiline: foo\s
    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS, style: :compact)
.foo { --multiline: foo bar; }
CSS
.foo {
  --multiline: foo\s


    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS, style: :compressed)
.foo{--multiline: foo bar}
CSS
.foo {
  --multiline: foo\s
    bar;
}
SCSS

    assert_equal <<CSS, render(<<SCSS, style: :compressed)
.foo{--multiline: foo bar}
CSS
.foo {
  --multiline: foo\s


    bar;
}
SCSS
  end

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
