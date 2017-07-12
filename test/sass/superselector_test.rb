require File.dirname(__FILE__) + '/../test_helper'

class SuperselectorTest < MiniTest::Test
  def test_superselector_reflexivity
    assert_superselector 'h1', 'h1'
    assert_superselector '.foo', '.foo'
    assert_superselector '#foo > .bar, baz', '#foo > .bar, baz'
  end

  def test_smaller_compound_superselector
    assert_strict_superselector '.foo', '.foo.bar'
    assert_strict_superselector '.bar', '.foo.bar'
    assert_strict_superselector 'a', 'a#b'
    assert_strict_superselector '#b', 'a#b'
  end

  def test_smaller_complex_superselector
    assert_strict_superselector '.bar', '.foo .bar'
    assert_strict_superselector '.bar', '.foo > .bar'
    assert_strict_superselector '.bar', '.foo + .bar'
    assert_strict_superselector '.bar', '.foo ~ .bar'
  end

  def test_selector_list_subset_superselector
    assert_strict_superselector '.foo, .bar', '.foo'
    assert_strict_superselector '.foo, .bar, .baz', '.foo, .baz'
    assert_strict_superselector '.foo, .baz, .qux', '.foo.bar, .baz.bang'
  end

  def test_leading_combinator_superselector
    refute_superselector '+ .foo', '.foo'
    refute_superselector '+ .foo', '.bar + .foo'
  end

  def test_trailing_combinator_superselector
    refute_superselector '.foo +', '.foo'
    refute_superselector '.foo +', '.foo + .bar'
  end

  def test_matching_combinator_superselector
    assert_strict_superselector '.foo + .bar', '.foo + .bar.baz'
    assert_strict_superselector '.foo + .bar', '.foo.baz + .bar'
    assert_strict_superselector '.foo > .bar', '.foo > .bar.baz'
    assert_strict_superselector '.foo > .bar', '.foo.baz > .bar'
    assert_strict_superselector '.foo ~ .bar', '.foo ~ .bar.baz'
    assert_strict_superselector '.foo ~ .bar', '.foo.baz ~ .bar'
  end

  def test_following_sibling_is_superselector_of_next_sibling
    assert_strict_superselector '.foo ~ .bar', '.foo + .bar.baz'
    assert_strict_superselector '.foo ~ .bar', '.foo.baz + .bar'
  end

  def test_descendant_is_superselector_of_child
    assert_strict_superselector '.foo .bar', '.foo > .bar.baz'
    assert_strict_superselector '.foo .bar', '.foo.baz > .bar'
    assert_strict_superselector '.foo .baz', '.foo > .bar > .baz'
  end

  def test_child_isnt_superselector_of_longer_child
    refute_superselector '.foo > .baz', '.foo > .bar > .baz'
    refute_superselector '.foo > .baz', '.foo > .bar .baz'
  end

  def test_following_sibling_isnt_superselector_of_longer_following_sibling
    refute_superselector '.foo + .baz', '.foo + .bar + .baz'
    refute_superselector '.foo + .baz', '.foo + .bar .baz'
  end

  def test_sibling_isnt_superselector_of_longer_sibling
    # This actually is a superselector, but it's a very narrow edge case and
    # detecting it is very difficult and may be exponential in the worst case.
    refute_superselector '.foo ~ .baz', '.foo ~ .bar ~ .baz'

    refute_superselector '.foo ~ .baz', '.foo ~ .bar .baz'
  end

  def test_matches_is_superselector_of_constituent_selectors
    %w[matches -moz-any].each do |name|
      assert_strict_superselector ":#{name}(.foo, .bar)", '.foo.baz'
      assert_strict_superselector ":#{name}(.foo, .bar)", '.bar.baz'
      assert_strict_superselector ":#{name}(.foo .bar, .baz)", '.x .foo .bar'
    end
  end

  def test_matches_is_superselector_of_subset_matches
    assert_strict_superselector ':matches(.foo, .bar, .baz)', '#x:matches(.foo.bip, .baz.bang)'
    assert_strict_superselector ':-moz-any(.foo, .bar, .baz)', '#x:-moz-any(.foo.bip, .baz.bang)'
  end

  def test_matches_is_not_superselector_of_any
    refute_superselector ':matches(.foo, .bar)', ':-moz-any(.foo, .bar)'
    refute_superselector ':-moz-any(.foo, .bar)', ':matches(.foo, .bar)'
  end

  def test_matches_can_be_subselector
    %w[matches -moz-any].each do |name|
      assert_superselector '.foo', ":#{name}(.foo.bar)"
      assert_superselector '.foo.bar', ":#{name}(.foo.bar.baz)"
      assert_superselector '.foo', ":#{name}(.foo.bar, .foo.baz)"
    end
  end

  def test_any_is_not_superselector_of_different_prefix
    refute_superselector ':-moz-any(.foo, .bar)', ':-s-any(.foo, .bar)'
  end

  def test_not_is_superselector_of_less_complex_not
    assert_strict_superselector ':not(.foo.bar)', ':not(.foo)'
    assert_strict_superselector ':not(.foo .bar)', ':not(.bar)'
  end

  def test_not_is_superselector_of_superset
    assert_strict_superselector ':not(.foo.bip, .baz.bang)', ':not(.foo, .bar, .baz)'
    assert_strict_superselector ':not(.foo.bip, .baz.bang)', ':not(.foo):not(.bar):not(.baz)'
  end

  def test_not_is_superselector_of_unique_selectors
    assert_strict_superselector ':not(h1.foo)', 'a'
    assert_strict_superselector ':not(.baz #foo)', '#bar'
  end

  def test_not_is_not_superselector_of_non_unique_selectors
    refute_superselector ':not(.foo)', '.bar'
    refute_superselector ':not(:hover)', ':visited'
  end

  def test_current_is_superselector_with_identical_innards
    assert_superselector ':current(.foo)', ':current(.foo)'
  end

  def test_current_is_superselector_with_subselector_innards
    refute_superselector ':current(.foo)', ':current(.foo.bar)'
    refute_superselector ':current(.foo.bar)', ':current(.foo)'
  end

  def test_nth_match_is_superselector_of_subset_nth_match
    assert_strict_superselector(
      ':nth-child(2n of .foo, .bar, .baz)', '#x:nth-child(2n of .foo.bip, .baz.bang)')
    assert_strict_superselector(
      ':nth-last-child(2n of .foo, .bar, .baz)', '#x:nth-last-child(2n of .foo.bip, .baz.bang)')
  end

  def test_nth_match_is_not_superselector_of_nth_match_with_different_arg
    refute_superselector(
      ':nth-child(2n of .foo, .bar, .baz)', '#x:nth-child(2n + 1 of .foo.bip, .baz.bang)')
    refute_superselector(
      ':nth-last-child(2n of .foo, .bar, .baz)', '#x:nth-last-child(2n + 1 of .foo.bip, .baz.bang)')
  end

  def test_nth_match_is_not_superselector_of_nth_last_match
    refute_superselector ':nth-child(2n of .foo, .bar)', ':nth-last-child(2n of .foo, .bar)'
    refute_superselector ':nth-last-child(2n of .foo, .bar)', ':nth-child(2n of .foo, .bar)'
  end

  def test_nth_match_can_be_subselector
    %w[nth-child nth-last-child].each do |name|
      assert_superselector '.foo', ":#{name}(2n of .foo.bar)"
      assert_superselector '.foo.bar', ":#{name}(2n of .foo.bar.baz)"
      assert_superselector '.foo', ":#{name}(2n of .foo.bar, .foo.baz)"
    end
  end

  def has_is_superselector_of_subset_host
    assert_strict_superselector ':has(.foo, .bar, .baz)', ':has(.foo.bip, .baz.bang)'
  end

  def has_isnt_superselector_of_contained_selector
    assert_strict_superselector ':has(.foo, .bar, .baz)', '.foo'
  end

  def host_is_superselector_of_subset_host
    assert_strict_superselector ':host(.foo, .bar, .baz)', ':host(.foo.bip, .baz.bang)'
  end

  def host_isnt_superselector_of_contained_selector
    assert_strict_superselector ':host(.foo, .bar, .baz)', '.foo'
  end

  def host_context_is_superselector_of_subset_host
    assert_strict_superselector(
      ':host-context(.foo, .bar, .baz)', ':host-context(.foo.bip, .baz.bang)')
  end

  def host_context_isnt_superselector_of_contained_selector
    assert_strict_superselector ':host-context(.foo, .bar, .baz)', '.foo'
  end

  private

  def assert_superselector(superselector, subselector)
    assert(parse_selector(superselector).superselector?(parse_selector(subselector)),
      "Expected #{superselector} to be a superselector of #{subselector}.")
  end

  def refute_superselector(superselector, subselector)
    assert(!parse_selector(superselector).superselector?(parse_selector(subselector)),
      "Expected #{superselector} not to be a superselector of #{subselector}.")
  end

  def assert_strict_superselector(superselector, subselector)
    assert_superselector(superselector, subselector)
    refute_superselector(subselector, superselector)
  end

  def parse_selector(selector)
    Sass::SCSS::CssParser.new(selector, filename_for_test, nil).parse_selector
  end
end
