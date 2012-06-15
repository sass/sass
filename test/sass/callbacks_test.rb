#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/callbacks'

class CallerBack
  extend Sass::Callbacks
  define_callback :foo
  define_callback :bar

  def do_foo
    run_foo
  end

  def do_bar
    run_bar 12
  end
end

module ClassLevelCallerBack
  extend Sass::Callbacks
  define_callback :foo
  extend self

  def do_foo
    run_foo
  end
end

class SassCallbacksTest < Test::Unit::TestCase
  def test_simple_callback
    cb = CallerBack.new
    there = false
    cb.on_foo {there = true}
    cb.do_foo
    assert there, "Expected callback to be called."
  end

  def test_multiple_callbacks
    cb = CallerBack.new
    str = ""
    cb.on_foo {str += "first"}
    cb.on_foo {str += " second"}
    cb.do_foo
    assert_equal "first second", str
  end

  def test_callback_with_arg
    cb = CallerBack.new
    val = nil
    cb.on_bar {|a| val = a}
    cb.do_bar
    assert_equal 12, val
  end

  def test_class_level_callback
    there = false
    ClassLevelCallerBack.on_foo {there = true}
    ClassLevelCallerBack.do_foo
    assert there, "Expected callback to be called."
  end
end
