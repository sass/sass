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

  def test_funcall_with_keyword_args
    assert_renders "foo(arg1, arg2, $karg1: val, $karg2: val2)"
    assert_renders "foo($karg1: val, $karg2: val2)"
  end

  def test_url
    assert_renders "url(foo.gif)"
    assert_renders "url($var)"
    assert_renders "url(\#{$var}/flip.gif)"
  end

  def test_variable
    assert_renders "$foo-bar"
    assert_renders "$flaznicate"
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

  def self.assert_associative(op_name, sibling_name)
    op = separator_for(op_name)
    sibling = separator_for(sibling_name)
    class_eval <<RUBY
      def test_associative_#{op_name}_#{sibling_name} 
        assert_renders "$foo#{op}$bar#{op}$baz"

        assert_equal "$foo#{op}$bar#{op}$baz",
          render("$foo#{op}($bar#{op}$baz)")
        assert_equal "$foo#{op}$bar#{op}$baz",
          render("($foo#{op}$bar)#{op}$baz")

        assert_equal "$foo#{op}$bar#{sibling}$baz",
          render("$foo#{op}($bar#{sibling}$baz)")
        assert_equal "$foo#{sibling}$bar#{op}$baz",
          render("($foo#{sibling}$bar)#{op}$baz")
      end
RUBY
  end

  def self.separator_for(op_name)
    case op_name
    when :comma; ", "
    when :space; " "
    else; " #{Sass::Script::Lexer::OPERATORS_REVERSE[op_name]} "
    end
  end

  def self.assert_non_associative(op_name, sibling_name)
    op = Sass::Script::Lexer::OPERATORS_REVERSE[op_name]
    sibling = Sass::Script::Lexer::OPERATORS_REVERSE[sibling_name]
    class_eval <<RUBY
      def test_non_associative_#{op_name}_#{sibling_name} 
        assert_renders "$foo #{op} $bar #{op} $baz"

        assert_renders "$foo #{op} ($bar #{op} $baz)"
        assert_equal "$foo #{op} $bar #{op} $baz",
          render("($foo #{op} $bar) #{op} $baz")

        assert_renders "$foo #{op} ($bar #{sibling} $baz)"
        assert_equal "$foo #{sibling} $bar #{op} $baz",
          render("($foo #{sibling} $bar) #{op} $baz")
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

  assert_associative :plus, :minus
  assert_associative :times, :div
  assert_associative :times, :mod

  assert_non_associative :minus, :plus
  assert_non_associative :div, :times
  assert_non_associative :mod, :times
  assert_non_associative :gt, :gte
  assert_non_associative :gte, :lt
  assert_non_associative :lt, :lte
  assert_non_associative :lte, :gt

  def test_comma_precedence
    assert_renders "$foo, $bar, $baz"

    assert_renders "$foo ($bar, $baz)"
    assert_renders "($foo, $bar) $baz"

    assert_equal "$foo, $bar $baz", render("$foo, ($bar $baz)")
    assert_equal "$foo $bar, $baz", render("($foo $bar), $baz")

    assert_equal "$foo, ($bar, $baz)", render("$foo, ($bar, $baz)")
    assert_equal "($foo, $bar), $baz", render("($foo, $bar), $baz")
  end

  def test_space_precedence
    assert_renders "$foo $bar $baz"

    assert_renders "$foo or ($bar $baz)"
    assert_renders "($foo $bar) or $baz"

    assert_equal "$foo $bar or $baz", render("$foo ($bar or $baz)")
    assert_equal "$foo or $bar $baz", render("($foo or $bar) $baz")

    assert_equal "$foo ($bar $baz)", render("$foo ($bar $baz)")
    assert_equal "($foo $bar) $baz", render("($foo $bar) $baz")
  end

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

  def test_interpolation_in_function
    assert_renders 'flabnabbit(#{1 + "foo"})'
    assert_renders 'flabnabbit($foo #{1 + "foo"}$baz)'
    assert_renders 'flabnabbit($foo #{1 + "foo"}#{2 + "bar"} $baz)'
  end

  def test_interpolation_near_operators
    assert_renders '#{1 + 2} , #{3 + 4}'
    assert_renders '#{1 + 2}, #{3 + 4}'
    assert_renders '#{1 + 2} ,#{3 + 4}'
    assert_renders '#{1 + 2},#{3 + 4}'
    assert_renders '#{1 + 2}, #{3 + 4}, #{5 + 6}'
    assert_renders '3, #{3 + 4}, 11'

    assert_renders '3 / #{3 + 4}'
    assert_renders '3 /#{3 + 4}'
    assert_renders '3/ #{3 + 4}'
    assert_renders '3/#{3 + 4}'

    assert_renders '#{1 + 2} * 7'
    assert_renders '#{1 + 2}* 7'
    assert_renders '#{1 + 2} *7'
    assert_renders '#{1 + 2}*7'

    assert_renders '-#{1 + 2}'
    assert_renders '- #{1 + 2}'

    assert_renders '5 + #{1 + 2} * #{3 + 4}'
    assert_renders '5 +#{1 + 2} * #{3 + 4}'
    assert_renders '5+#{1 + 2} * #{3 + 4}'
    assert_renders '#{1 + 2} * #{3 + 4} + 5'
    assert_renders '#{1 + 2} * #{3 + 4}+ 5'
    assert_renders '#{1 + 2} * #{3 + 4}+5'

    assert_equal '5 / #{1 + 2} + #{3 + 4}', render('5 / (#{1 + 2} + #{3 + 4})')
    assert_equal '5 / #{1 + 2} + #{3 + 4}', render('5 /(#{1 + 2} + #{3 + 4})')
    assert_equal '5 / #{1 + 2} + #{3 + 4}', render('5 /( #{1 + 2} + #{3 + 4} )')
    assert_equal '#{1 + 2} + #{3 + 4} / 5', render('(#{1 + 2} + #{3 + 4}) / 5')
    assert_equal '#{1 + 2} + #{3 + 4} / 5', render('(#{1 + 2} + #{3 + 4})/ 5')
    assert_equal '#{1 + 2} + #{3 + 4} / 5', render('( #{1 + 2} + #{3 + 4} )/ 5')

    assert_renders '#{1 + 2} + 2 + 3'
    assert_renders '#{1 + 2} +2 + 3'
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

  private

  def assert_renders(script, options = {})
    assert_equal(script, render(script, options))
  end

  def render(script, options = {})
    munge_filename(options)
    node = Sass::Script.parse(script, 1, 0, options)
    node.to_sass
  end
end
