#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/util/normalized_map'

class NormalizedMapTest < Test::Unit::TestCase
  extend PublicApiLinter

  lint_api Hash, Sass::Util::NormalizedMap

  def lint_instance
    Sass::Util::NormalizedMap.new
  end

  def test_normalized_map_errors_unless_explicitly_implemented
    assert Sass.tests_running
    assert_raise_message(ArgumentError, "The method invert must be implemented explicitly") do
      Sass::Util::NormalizedMap.new.invert
    end
  end

  def test_normalized_map_does_not_error_when_released
    Sass.tests_running = false
    assert_equal({}, Sass::Util::NormalizedMap.new.invert)
  ensure
    Sass.tests_running = true
  end

  def test_basic_lifecycle
    m = Sass::Util::NormalizedMap.new
    m["a-b"] = 1
    assert_equal ["a_b"], m.keys
    assert_equal 1, m["a_b"]
    assert_equal 1, m["a-b"]
    assert m.has_key?("a_b")
    assert m.has_key?("a-b")
    assert_equal({"a-b" => 1}, m.as_stored)
    assert_equal 1, m.delete("a-b")
    assert !m.has_key?("a-b")
    m["a_b"] = 2
    assert_equal({"a_b" => 2}, m.as_stored)
  end

  def test_dup
    m = Sass::Util::NormalizedMap.new
    m["a-b"] = 1
    m2 = m.dup
    m.delete("a-b")
    assert !m.has_key?("a-b")
    assert m2.has_key?("a-b")
  end
end
