#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../../test_helper'

class SubsetMapTest < Test::Unit::TestCase
  def setup
    @ssm = Haml::Util::SubsetMap.new
    @ssm[Set[1, 2]] = "Foo"
    @ssm[Set["fizz", "fazz"]] = "Bar"

    @ssm[Set[:foo, :bar]] = "Baz"
    @ssm[Set[:foo, :bar, :baz]] = "Bang"

    @ssm[Set[:bip, :bop, :blip]] = "Qux"
    @ssm[Set[:bip, :bop]] = "Thram"
  end

  def test_equal_keys
    assert_equal ["Foo"], @ssm[Set[1, 2]]
    assert_equal ["Bar"], @ssm[Set["fizz", "fazz"]]
  end

  def test_subset_keys
    assert_equal ["Foo"], @ssm[Set[1, 2, "fuzz"]]
    assert_equal ["Bar"], @ssm[Set["fizz", "fazz", 3]]
  end

  def test_superset_keys
    assert_equal [], @ssm[Set[1]]
    assert_equal [], @ssm[Set[2]]
    assert_equal [], @ssm[Set["fizz"]]
    assert_equal [], @ssm[Set["fazz"]]
  end

  def test_disjoint_keys
    assert_equal [], @ssm[Set[3, 4]]
    assert_equal [], @ssm[Set["fuzz", "frizz"]]
    assert_equal [], @ssm[Set["gran", 15]]
  end

  def test_semi_disjoint_keys
    assert_equal [], @ssm[Set[2, 3]]
    assert_equal [], @ssm[Set["fizz", "fuzz"]]
    assert_equal [], @ssm[Set[1, "fazz"]]
  end

  def test_empty_key_set
    assert_raise(ArgumentError) {@ssm[Set[]] = "Fail"}
  end

  def test_empty_key_get
    assert_equal [], @ssm[Set[]]
  end

  def test_multiple_subsets
    assert_equal ["Foo", "Bar"], @ssm[Set[1, 2, "fizz", "fazz"]]
    assert_equal ["Foo", "Bar"], @ssm[Set[1, 2, 3, "fizz", "fazz", "fuzz"]]

    assert_equal ["Baz"], @ssm[Set[:foo, :bar]]
    assert_equal ["Baz", "Bang"], @ssm[Set[:foo, :bar, :baz]]
  end

  def test_order_preserved
    @ssm[Set[10, 11, 12]] = 1
    @ssm[Set[10, 11]] = 2
    @ssm[Set[11]] = 3
    @ssm[Set[11, 12]] = 4
    @ssm[Set[9, 10, 11, 12, 13]] = 5
    @ssm[Set[10, 13]] = 6

    assert_equal [1, 2, 3, 4, 5, 6], @ssm[Set[9, 10, 11, 12, 13]]
  end

  def test_multiple_equal_values
    @ssm[Set[11, 12]] = 1
    @ssm[Set[12, 13]] = 2
    @ssm[Set[13, 14]] = 1
    @ssm[Set[14, 15]] = 1

    assert_equal [1, 2, 1, 1], @ssm[Set[11, 12, 13, 14, 15]]
  end
end
