#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class SassScriptConversionTest < Test::Unit::TestCase
  def test_bool
    assert_renders "true"
    assert_renders "false"
  end

  def test_color
    assert_renders "#abcdef"
    assert_renders "blue"
    assert_renders "rgba(0, 1, 2, 0.2)"

    assert_equal "#aabbcc", render("#abc")
    assert_equal "blue", render("#0000ff")
  end

  def test_number
    assert_renders "10"
    assert_renders "10.35"
    assert_renders "12px"
    assert_renders "12.45px"

    assert_equal "12.346", render("12.345678901")
  end

  def test_string
    assert_renders '"foo"'
    assert_renders '"bar baz"'
    assert_equal '"baz bang"', render("'baz bang'")
  end

  def test_string_quotes
    assert_equal "'quote\"quote'", render('"quote\\"quote"')
    assert_equal '"quote\'quote"', render("'quote\\'quote'")
    assert_renders '"quote\'quote\\"quote"'
    assert_equal '"quote\'quote\\"quote"', render("'quote\\'quote\"quote'")
  end

  def test_string_escapes
    assert_renders '"foo\\\\bar"'
  end

  def test_funcall
    assert_renders "foo(true, blue)"
    assert_renders "hsla(20deg, 30%, 50%, 0.3)"
    assert_renders "blam()"

    assert_renders "-\xC3\xBFoo(12px)"
    assert_renders "-foo(12px)"
  end

  def test_url
    assert_renders "url(foo.gif)"
    assert_renders "url($var)"
    assert_renders "url(\#{$var}/flip.gif)"
  end

  def test_variable
    assert_renders "$foo-bar"
    assert_renders "$flaznicate"
    assert_warning(<<WARN) {assert_equal "$tumbly-wumbly", render("!tumbly-wumbly")}
DEPRECATION WARNING:
On line 1, character 1 of 'test_variable_inline.sass'
Variables with ! have been deprecated and will be removed in version 3.2.
Use "$tumbly-wumbly" instead.

You can use `sass-convert --in-place --from sass2 file.sass' to convert files automatically.
WARN
  end

  def test_important
    assert_renders "!important"
    assert_renders "$foo !important"
  end

  def test_comma_operator
    assert_renders "$foo, $bar $baz"
    assert_renders "$foo $bar, $baz"

    assert_renders "($foo, $bar) $baz"
    assert_renders "$foo ($bar, $baz)"

    assert_equal "$foo, $bar $baz", render("$foo, ($bar $baz)")
    assert_equal "$foo $bar, $baz", render("($foo $bar), $baz")
  end

  def test_concat_operator
    assert_renders "$foo $bar or $baz"
    assert_renders "$foo or $bar $baz"

    assert_renders "($foo $bar) or $baz"
    assert_renders "$foo or ($bar $baz)"

    assert_equal "$foo $bar or $baz", render("$foo ($bar or $baz)")
    assert_equal "$foo or $bar $baz", render("($foo or $bar) $baz")
  end

  def self.test_precedence(outer, inner)
    op_outer = Sass::Script::Lexer::OPERATORS_REVERSE[outer]
    op_inner = Sass::Script::Lexer::OPERATORS_REVERSE[inner]
    class_eval <<RUBY
      def test_precedence_#{outer}_#{inner} 
        assert_renders "$foo #{op_outer} $bar #{op_inner} $baz"
        assert_renders "$foo #{op_inner} $bar #{op_outer} $baz"

        assert_renders "($foo #{op_outer} $bar) #{op_inner} $baz"
        assert_renders "$foo #{op_inner} ($bar #{op_outer} $baz)"

        assert_equal "$foo #{op_outer} $bar #{op_inner} $baz",
          render("$foo #{op_outer} ($bar #{op_inner} $baz)")
        assert_equal "$foo #{op_inner} $bar #{op_outer} $baz",
          render("($foo #{op_inner} $bar) #{op_outer} $baz")
      end
RUBY
  end

  test_precedence :or, :and
  test_precedence :and, :eq
  test_precedence :and, :neq
  test_precedence :eq, :gt
  test_precedence :eq, :gte
  test_precedence :eq, :lt
  test_precedence :eq, :lte
  test_precedence :gt, :plus
  test_precedence :gt, :minus
  test_precedence :plus, :times
  test_precedence :plus, :div
  test_precedence :plus, :mod

  def test_unary_op
    assert_renders "-12px"
    assert_renders '/"foo"'
    assert_renders 'not true'

    assert_renders "-(foo(12px))"
    assert_renders "-(-foo(12px))"
    assert_renders "-(_foo(12px))"
    assert_renders "-(\xC3\xBFoo(12px))"
    assert_renders "-(blue)"

    assert_equal 'not true or false', render('(not true) or false')
    assert_equal 'not (true or false)', render('not (true or false)')
  end

  def test_interpolation
    assert_renders "$foo\#{$bar}$baz"
    assert_renders "$foo\#{$bar} $baz"
    assert_renders "$foo \#{$bar}$baz"
    assert_renders "$foo \#{$bar} $baz"
    assert_renders "$foo \#{$bar}\#{$bang} $baz"
    assert_renders "$foo \#{$bar} \#{$bang} $baz"
    assert_renders "\#{$bar}$baz"
    assert_renders "$foo\#{$bar}"
    assert_renders "\#{$bar}"
  end

  def test_string_interpolation
    assert_renders '"foo#{$bar}baz"'
    assert_renders '"foo #{$bar}baz"'
    assert_renders '"foo#{$bar} baz"'
    assert_renders '"foo #{$bar} baz"'
    assert_renders '"foo #{$bar}#{$bang} baz"'
    assert_renders '"foo #{$bar} #{$bang} baz"'
    assert_renders '"#{$bar}baz"'
    assert_renders '"foo#{$bar}"'
    assert_equal '#{$bar}', render('"#{$bar}"')

    assert_equal '"foo#{$bar}baz"', render("'foo\#{$bar}baz'")
  end

  def test_sass2_string_interpolation
    assert_equal 'foo#{$bar}baz', render('"foo#{$bar}baz"', :context => :equals)
    assert_equal '#{$bar}baz', render('"#{$bar}baz"', :context => :equals)
    assert_equal 'foo#{$bar}', render('"foo#{$bar}"', :context => :equals)

    assert_equal 'unquote(".foo#{$bar}.bar")', render('".foo#{$bar}.bar"', :context => :equals)
    assert_equal 'unquote(".foo#{$bar}")', render('".foo#{$bar}"', :context => :equals)
    assert_equal 'unquote("#{$bar}.bar")', render('"#{$bar}.bar"', :context => :equals)

    assert_equal "unquote(\"f'o\#{$bar}b'z\")", render("'f\\'o\#{$bar}b\\'z'", :context => :equals)
    assert_equal "unquote('f\"o\#{$bar}b\"z')", render("'f\\\"o\#{$bar}b\\\"z'", :context => :equals)
    assert_equal "unquote(\"f'o\#{$bar}b\\\"z\")", render("'f\\'o\#{$bar}b\\\"z'", :context => :equals)
  end

  def test_sass2_urls
    Haml::Util.silence_haml_warnings do
      assert_equal 'url(foo/bar.gif)', render('url(foo/bar.gif)', :context => :equals)
      assert_equal 'url("foo/bar.gif")', render('url("foo/bar.gif")', :context => :equals)

      assert_equal 'url($var)', render('url(!var)', :context => :equals)
      assert_equal 'url("#{$var}/flip.gif")', render('url("#{!var}/flip.gif")', :context => :equals)
    end
  end

  private

  def assert_renders(script, options = {})
    assert_equal(script, render(script, options))
  end

  def render(script, options = {})
    munge_filename(options)
    node = Sass::Script.parse(script, 1, 0, options)
    node.context = options[:context] if options[:context]
    node.to_sass
  end
end
