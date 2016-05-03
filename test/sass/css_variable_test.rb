#!/usr/bin/env ruby

require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class CssVariableTest < MiniTest::Test
  def test_simple_values
    assert_same_value('value')
    assert_same_value('value1 value2')
    assert_same_value('foo(bar)')
    assert_same_value('url(http://foo.com/bar)')
    assert_same_value('#foo')
    assert_same_value('12.6e7')
    assert_same_value('*/')
  end

  def test_empty_value
    # The indented syntax always ignores trailing whitespace, so only test this
    # for SCSS.
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar: ; }
CSS
.foo {
  --bar: ; }
SCSS
  end

  def test_nested_characters
    assert_same_value('(foo; (bar: baz;) bang!)')
    assert_same_value('{foo; (bar: baz;) bang!}')
    assert_same_value('[foo; (bar: baz;) bang!]')
    assert_same_value('[({{([])}})]')
  end

  def test_sass_script_doesnt_work
    assert_same_value('$variable')
    assert_same_value('1 + 1')
    assert_same_value('red(#ffffff)')
    assert_same_value('(a b c)')
    assert_same_value('(a: b, c: d)')
  end

  def test_strings_are_tokenized
    assert_same_value('"foo"')
    assert_same_value('"!"')
    assert_same_value('";"')
    assert_same_value('"]["')
    assert_same_value('"}{"')
    assert_same_value('")("')
  end

  def test_block_comments_are_tokenized
    assert_same_value('/*foo*/')
    assert_same_value('/* ! */')
    assert_same_value('/* ; */')
    assert_same_value('/* ][ */')
    assert_same_value('/* }{ */')
    assert_same_value('/* )( */')
    assert_same_value('/* /* */ */')
  end

  def test_block_comments_are_ignored
    assert_same_value('/*foo*/')
    assert_same_value('/* ! */')
    assert_same_value('/* ; */')
    assert_same_value('/* ][ */')
    assert_same_value('/* }{ */')
    assert_same_value('/* )( */')
    assert_same_value('/* /* */ */')
  end

  def test_single_line_comments_arent_treated_specially
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar: // (
    ); }
CSS
.foo {
  --bar: // (
    );
}
SCSS
  end

  def test_interpolation
    assert_variable_value('3', '#{1 + 2}')
    assert_variable_value('a 3 c', 'a #{1 + 2} c')
    assert_variable_value('foo3bar', 'foo#{1 + 2}bar')
    assert_variable_value('"foo3bar"', '"foo#{1 + 2}bar"')
    assert_variable_value('uri(foo3bar)', 'uri(foo#{1 + 2}bar)')
  end

  def test_extra_whitespace_isnt_added
    # Custom properties care whether there's whitespace before the first token.
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar:baz; }
CSS
.foo {
  --bar:baz;
}
SCSS

    assert_equal <<CSS, render(<<SASS, syntax: :sass)
.foo {
  --bar:baz; }
CSS
.foo
  --bar:baz
SASS
  end

  def test_trailing_whitespace_isnt_removed
    # The indented syntax always ignores trailing whitespace, so only test this
    # for SCSS.
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar:baz
; }
CSS
.foo {
  --bar:baz
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar:baz\t; }
CSS
.foo {
  --bar:baz\t;
}
SCSS
  end

  def test_nested_properties_arent_flattened
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar: {baz: bang;}; }
CSS
.foo {
  --bar: {baz: bang;};
}
SCSS
  end

  def test_ambiguous_declarations_are_always_properties
    assert_equal <<CSS, render(<<SCSS)
.foo {
  --bar:baz {bang: qux;}; }
CSS
.foo {
  --bar:baz {bang: qux;};
}
SCSS
  end

  # Conversion

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

  # Syntax errors

  def test_dont_allow_nesting
    assert_raise_message Sass::SyntaxError, <<ERROR.rstrip do
Illegal nesting: Nothing may be nested beneath custom properties.
ERROR
      render(<<SASS, syntax: :sass)
.foo
  --bar: baz
    a: b
SASS
    end

    assert_raise_message Sass::SyntaxError, <<ERROR.rstrip do
Illegal nesting: Nothing may be nested beneath custom properties.
ERROR
      render(<<SASS, syntax: :sass)
.foo
  --bar:
    a: b
SASS
    end
  end

  def test_disallow_unmatched_brackets
    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable(")")}
Invalid CSS after "  --variable: ": expected "}", was ");"
ERROR

    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("]")}
Invalid CSS after "  --variable: ": expected "}", was "];"
ERROR

    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("(])")}
Invalid CSS after "  --variable: (": expected ")", was "]);"
ERROR

    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("[}]")}
Invalid CSS after "  --variable: [": expected "]", was "}];"
ERROR

    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("{)}")}
Invalid CSS after "  --variable: {": expected "}", was ")};"
ERROR
  end

  def test_disallow_top_level_bang
    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("!")}
Invalid CSS after "  --variable: ": expected "}", was "!;"
ERROR

    assert_raise_message(Sass::SyntaxError, <<ERROR.rstrip) {render_variable("foo!bar")}
Invalid CSS after "  --variable: foo": expected "}", was "!bar;"
ERROR
  end

  private

  def assert_same_value(value)
    assert_variable_value(value, value)
  end

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
