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

  def test_dup
    m = Sass::Util::NormalizedMap.new
    m["a-b"] = 1
    m2 = m.dup
    m.delete("a-b")
    assert !m.has_key?("a-b")
    assert m2.has_key?("a-b")
  end

end
