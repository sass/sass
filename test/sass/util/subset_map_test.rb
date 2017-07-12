require File.dirname(__FILE__) + '/../../test_helper'

class SubsetMapTest < MiniTest::Test
  def setup
    @ssm = Sass::Util::SubsetMap.new
    @ssm[Set[1, 2]] = "Foo"
    @ssm[Set["fizz", "fazz"]] = "Bar"

    @ssm[Set[:foo, :bar]] = "Baz"
    @ssm[Set[:foo, :bar, :baz]] = "Bang"

    @ssm[Set[:bip, :bop, :blip]] = "Qux"
    @ssm[Set[:bip, :bop]] = "Thram"
  end

  def test_equal_keys
    assert_equal [["Foo", Set[1, 2]]], @ssm.get(Set[1, 2])
    assert_equal [["Bar", Set["fizz", "fazz"]]], @ssm.get(Set["fizz", "fazz"])
  end

  def test_subset_keys
    assert_equal [["Foo", Set[1, 2]]], @ssm.get(Set[1, 2, "fuzz"])
    assert_equal [["Bar", Set["fizz", "fazz"]]], @ssm.get(Set["fizz", "fazz", 3])
  end

  def test_superset_keys
    assert_equal [], @ssm.get(Set[1])
    assert_equal [], @ssm.get(Set[2])
    assert_equal [], @ssm.get(Set["fizz"])
    assert_equal [], @ssm.get(Set["fazz"])
  end

  def test_disjoint_keys
    assert_equal [], @ssm.get(Set[3, 4])
    assert_equal [], @ssm.get(Set["fuzz", "frizz"])
    assert_equal [], @ssm.get(Set["gran", 15])
  end

  def test_semi_disjoint_keys
    assert_equal [], @ssm.get(Set[2, 3])
    assert_equal [], @ssm.get(Set["fizz", "fuzz"])
    assert_equal [], @ssm.get(Set[1, "fazz"])
  end

  def test_empty_key_set
    assert_raises(ArgumentError) {@ssm[Set[]] = "Fail"}
  end

  def test_empty_key_get
    assert_equal [], @ssm.get(Set[])
  end

  def test_multiple_subsets
    assert_equal [["Foo", Set[1, 2]], ["Bar", Set["fizz", "fazz"]]], @ssm.get(Set[1, 2, "fizz", "fazz"])
    assert_equal [["Foo", Set[1, 2]], ["Bar", Set["fizz", "fazz"]]], @ssm.get(Set[1, 2, 3, "fizz", "fazz", "fuzz"])

    assert_equal [["Baz", Set[:foo, :bar]]], @ssm.get(Set[:foo, :bar])
    assert_equal [["Baz", Set[:foo, :bar]], ["Bang", Set[:foo, :bar, :baz]]], @ssm.get(Set[:foo, :bar, :baz])
  end

  def test_bracket_bracket
    assert_equal ["Foo"], @ssm[Set[1, 2, "fuzz"]]
    assert_equal ["Baz", "Bang"], @ssm[Set[:foo, :bar, :baz]]
  end

  def test_order_preserved
    @ssm[Set[10, 11, 12]] = 1
    @ssm[Set[10, 11]] = 2
    @ssm[Set[11]] = 3
    @ssm[Set[11, 12]] = 4
    @ssm[Set[9, 10, 11, 12, 13]] = 5
    @ssm[Set[10, 13]] = 6

    assert_equal(
      [[1, Set[10, 11, 12]], [2, Set[10, 11]], [3, Set[11]], [4, Set[11, 12]],
        [5, Set[9, 10, 11, 12, 13]], [6, Set[10, 13]]],
      @ssm.get(Set[9, 10, 11, 12, 13]))
  end

  def test_multiple_equal_values
    @ssm[Set[11, 12]] = 1
    @ssm[Set[12, 13]] = 2
    @ssm[Set[13, 14]] = 1
    @ssm[Set[14, 15]] = 1

    assert_equal(
      [[1, Set[11, 12]], [2, Set[12, 13]], [1, Set[13, 14]], [1, Set[14, 15]]],
      @ssm.get(Set[11, 12, 13, 14, 15]))
  end
end
