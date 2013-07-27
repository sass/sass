#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/util/normalized_map'

class NormalizedMapTest < Test::Unit::TestCase

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

  def test_initialize_with_hash
    m = Sass::Util::NormalizedMap.new("a-b" => Object.new)
    assert m.has_key?("a_b")
  end

  def test_dup
    m = Sass::Util::NormalizedMap.new
    m["a-b"] = 1
    m2 = m.dup
    m.delete("a-b")
    assert !m.has_key?("a-b")
    assert m2.has_key?("a-b")
  end

  def test_deep_copy
    m = Sass::Util::NormalizedMap.new
    deep_copyable = Object.new
    def deep_copyable.deep_copy
      @deep_copy_called = true
      dup
    end
    def deep_copyable.deep_copy_called?
      @deep_copy_called
    end
    m["a-b"] = 1
    m["b-c"] = Object.new
    m["c-d"] = deep_copyable
    m2 = m.deep_copy
    m.delete("a-b")
    assert !m.has_key?("a-b")
    assert m2.has_key?("a-b")
    assert deep_copyable.deep_copy_called?
  end
end
