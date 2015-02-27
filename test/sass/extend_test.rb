#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ExtendTest < MiniTest::Test
  def test_basic
    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }
CSS
.foo {a: b}
.bar {@extend .foo}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }
CSS
.bar {@extend .foo}
.foo {a: b}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }

.bar {
  c: d; }
CSS
.foo {a: b}
.bar {c: d; @extend .foo}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }

.bar {
  c: d; }
CSS
.foo {a: b}
.bar {@extend .foo; c: d}
SCSS
  end

  def test_indented_syntax
    assert_equal <<CSS, render(<<SASS, :syntax => :sass)
.foo, .bar {
  a: b; }
CSS
.foo
  a: b
.bar
  @extend .foo
SASS

    assert_equal <<CSS, render(<<SASS, :syntax => :sass)
.foo, .bar {
  a: b; }
CSS
.foo
  a: b
.bar
  @extend \#{".foo"}
SASS
  end

  def test_multiple_targets
    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }

.blip .foo, .blip .bar {
  c: d; }
CSS
.foo {a: b}
.bar {@extend .foo}
.blip .foo {c: d}
SCSS
  end

  def test_multiple_extendees
    assert_equal <<CSS, render(<<SCSS)
.foo, .baz {
  a: b; }

.bar, .baz {
  c: d; }
CSS
.foo {a: b}
.bar {c: d}
.baz {@extend .foo; @extend .bar}
SCSS
  end

  def test_multiple_extends_with_single_extender_and_single_target
    assert_extends('.foo .bar', '.baz {@extend .foo; @extend .bar}',
      '.foo .bar, .baz .bar, .foo .baz, .baz .baz')
    assert_extends '.foo.bar', '.baz {@extend .foo; @extend .bar}', '.foo.bar, .baz'
  end

  def test_multiple_extends_with_multiple_extenders_and_single_target
    assert_equal <<CSS, render(<<SCSS)
.foo .bar, .baz .bar, .foo .bang, .baz .bang {
  a: b; }
CSS
.foo .bar {a: b}
.baz {@extend .foo}
.bang {@extend .bar}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo.bar, .bar.baz, .baz.bang, .foo.bang {
  a: b; }
CSS
.foo.bar {a: b}
.baz {@extend .foo}
.bang {@extend .bar}
SCSS
  end

  def test_chained_extends
    assert_equal <<CSS, render(<<SCSS)
.foo, .bar, .baz, .bip {
  a: b; }
CSS
.foo {a: b}
.bar {@extend .foo}
.baz {@extend .bar}
.bip {@extend .bar}
SCSS
  end

  def test_dynamic_extendee
    assert_extends '.foo', '.bar {@extend #{".foo"}}', '.foo, .bar'
    assert_extends('[baz^="blip12px"]', '.bar {@extend [baz^="blip#{12px}"]}',
      '[baz^="blip12px"], .bar')
  end

  def test_nested_target
    assert_extends '.foo .bar', '.baz {@extend .bar}', '.foo .bar, .foo .baz'
  end

  def test_target_with_child
    assert_extends '.foo .bar', '.baz {@extend .foo}', '.foo .bar, .baz .bar'
  end

  def test_class_unification
    assert_unification '.foo.bar', '.baz {@extend .foo}', '.foo.bar, .bar.baz'
    assert_unification '.foo.baz', '.baz {@extend .foo}', '.baz'
  end

  def test_id_unification
    assert_unification '.foo.bar', '#baz {@extend .foo}', '.foo.bar, .bar#baz'
    assert_unification '.foo#baz', '#baz {@extend .foo}', '#baz'

    assert_extend_doesnt_match('#bar', '.foo', :failed_to_unify, 2) do
      render_unification '.foo#baz', '#bar {@extend .foo}'
    end
  end

  def test_universal_unification_with_simple_target
    assert_unification '.foo', '* {@extend .foo}', '.foo, *'
    assert_unification '.foo', '*|* {@extend .foo}', '.foo, *|*'
    assert_unification '.foo.bar', '* {@extend .foo}', '.bar'
    assert_unification '.foo.bar', '*|* {@extend .foo}', '.bar'
    assert_unification '.foo.bar', 'ns|* {@extend .foo}', '.foo.bar, ns|*.bar'
  end

  def test_universal_unification_with_namespaceless_universal_target
    assert_unification '*.foo', '* {@extend .foo}', '*'
    assert_unification '*.foo', '*|* {@extend .foo}', '*'
    assert_unification '*|*.foo', '* {@extend .foo}', '*|*.foo, *'
    assert_unification '*|*.foo', '*|* {@extend .foo}', '*|*'
    assert_unification '*.foo', 'ns|* {@extend .foo}', '*.foo, ns|*'
    assert_unification '*|*.foo', 'ns|* {@extend .foo}', '*|*.foo, ns|*'
  end

  def test_universal_unification_with_namespaced_universal_target
    assert_unification 'ns|*.foo', '* {@extend .foo}', 'ns|*'
    assert_unification 'ns|*.foo', '*|* {@extend .foo}', 'ns|*'

    assert_extend_doesnt_match('ns2|*', '.foo', :failed_to_unify, 2) do
      render_unification 'ns1|*.foo', 'ns2|* {@extend .foo}'
    end

    assert_unification 'ns|*.foo', 'ns|* {@extend .foo}', 'ns|*'
  end

  def test_universal_unification_with_namespaceless_element_target
    assert_unification 'a.foo', '* {@extend .foo}', 'a'
    assert_unification 'a.foo', '*|* {@extend .foo}', 'a'
    assert_unification '*|a.foo', '* {@extend .foo}', '*|a.foo, a'
    assert_unification '*|a.foo', '*|* {@extend .foo}', '*|a'
    assert_unification 'a.foo', 'ns|* {@extend .foo}', 'a.foo, ns|a'
    assert_unification '*|a.foo', 'ns|* {@extend .foo}', '*|a.foo, ns|a'
  end

  def test_universal_unification_with_namespaced_element_target
    assert_unification 'ns|a.foo', '* {@extend .foo}', 'ns|a'
    assert_unification 'ns|a.foo', '*|* {@extend .foo}', 'ns|a'

    assert_extend_doesnt_match('ns2|*', '.foo', :failed_to_unify, 2) do
      render_unification 'ns1|a.foo', 'ns2|* {@extend .foo}'
    end

    assert_unification 'ns|a.foo', 'ns|* {@extend .foo}', 'ns|a'
  end

  def test_element_unification_with_simple_target
    assert_unification '.foo', 'a {@extend .foo}', '.foo, a'
    assert_unification '.foo.bar', 'a {@extend .foo}', '.foo.bar, a.bar'
    assert_unification '.foo.bar', '*|a {@extend .foo}', '.foo.bar, *|a.bar'
    assert_unification '.foo.bar', 'ns|a {@extend .foo}', '.foo.bar, ns|a.bar'
  end

  def test_element_unification_with_namespaceless_universal_target
    assert_unification '*.foo', 'a {@extend .foo}', '*.foo, a'
    assert_unification '*.foo', '*|a {@extend .foo}', '*.foo, a'
    assert_unification '*|*.foo', 'a {@extend .foo}', '*|*.foo, a'
    assert_unification '*|*.foo', '*|a {@extend .foo}', '*|*.foo, *|a'
    assert_unification '*.foo', 'ns|a {@extend .foo}', '*.foo, ns|a'
    assert_unification '*|*.foo', 'ns|a {@extend .foo}', '*|*.foo, ns|a'
  end

  def test_element_unification_with_namespaced_universal_target
    assert_unification 'ns|*.foo', 'a {@extend .foo}', 'ns|*.foo, ns|a'
    assert_unification 'ns|*.foo', '*|a {@extend .foo}', 'ns|*.foo, ns|a'

    assert_extend_doesnt_match('ns2|a', '.foo', :failed_to_unify, 2) do
      render_unification 'ns1|*.foo', 'ns2|a {@extend .foo}'
    end

    assert_unification 'ns|*.foo', 'ns|a {@extend .foo}', 'ns|*.foo, ns|a'
  end

  def test_element_unification_with_namespaceless_element_target
    assert_unification 'a.foo', 'a {@extend .foo}', 'a'
    assert_unification 'a.foo', '*|a {@extend .foo}', 'a'
    assert_unification '*|a.foo', 'a {@extend .foo}', '*|a.foo, a'
    assert_unification '*|a.foo', '*|a {@extend .foo}', '*|a'
    assert_unification 'a.foo', 'ns|a {@extend .foo}', 'a.foo, ns|a'
    assert_unification '*|a.foo', 'ns|a {@extend .foo}', '*|a.foo, ns|a'

    assert_extend_doesnt_match('h1', '.foo', :failed_to_unify, 2) do
      render_unification 'a.foo', 'h1 {@extend .foo}'
    end
  end

  def test_element_unification_with_namespaced_element_target
    assert_unification 'ns|a.foo', 'a {@extend .foo}', 'ns|a'
    assert_unification 'ns|a.foo', '*|a {@extend .foo}', 'ns|a'

    assert_extend_doesnt_match('ns2|a', '.foo', :failed_to_unify, 2) do
      render_unification 'ns1|a.foo', 'ns2|a {@extend .foo}'
    end

    assert_unification 'ns|a.foo', 'ns|a {@extend .foo}', 'ns|a'
  end

  def test_attribute_unification
    assert_unification '[foo=bar].baz', '[foo=baz] {@extend .baz}', '[foo=bar].baz, [foo=bar][foo=baz]'
    assert_unification '[foo=bar].baz', '[foo^=bar] {@extend .baz}', '[foo=bar].baz, [foo=bar][foo^=bar]'
    assert_unification '[foo=bar].baz', '[foot=bar] {@extend .baz}', '[foo=bar].baz, [foo=bar][foot=bar]'
    assert_unification '[foo=bar].baz', '[ns|foo=bar] {@extend .baz}', '[foo=bar].baz, [foo=bar][ns|foo=bar]'
    assert_unification '%-a [foo=bar].bar', '[foo=bar] {@extend .bar}', '-a [foo=bar]'
  end

  def test_pseudo_unification
    assert_unification ':foo.baz', ':foo(2n+1) {@extend .baz}', ':foo.baz, :foo:foo(2n+1)'
    assert_unification ':foo.baz', '::foo {@extend .baz}', ':foo.baz, :foo::foo'

    assert_extend_doesnt_match('::bar', '.baz', :failed_to_unify, 2) do
      render_unification '::foo.baz', '::bar {@extend .baz}'
    end

    assert_extend_doesnt_match('::foo(2n+1)', '.baz', :failed_to_unify, 2) do
      render_unification '::foo.baz', '::foo(2n+1) {@extend .baz}'
    end

    assert_unification '::foo.baz', '::foo {@extend .baz}', '::foo'
    assert_unification '::foo(2n+1).baz', '::foo(2n+1) {@extend .baz}', '::foo(2n+1)'
    assert_unification ':foo.baz', ':bar {@extend .baz}', ':foo.baz, :foo:bar'
    assert_unification '.baz:foo', ':after {@extend .baz}', '.baz:foo, :foo:after'
    assert_unification '.baz:after', ':foo {@extend .baz}', '.baz:after, :foo:after'
    assert_unification ':foo.baz', ':foo {@extend .baz}', ':foo'
  end

  def test_pseudoelement_remains_at_end_of_selector
    assert_extends '.foo::bar', '.baz {@extend .foo}', '.foo::bar, .baz::bar'
    assert_extends 'a.foo::bar', '.baz {@extend .foo}', 'a.foo::bar, a.baz::bar'
  end

  def test_pseudoclass_remains_at_end_of_selector
    assert_extends '.foo:bar', '.baz {@extend .foo}', '.foo:bar, .baz:bar'
    assert_extends 'a.foo:bar', '.baz {@extend .foo}', 'a.foo:bar, a.baz:bar'
  end

  def test_not_remains_at_end_of_selector
    assert_extends '.foo:not(.bar)', '.baz {@extend .foo}', '.foo:not(.bar), .baz:not(.bar)'
  end

  def test_pseudoelement_goes_lefter_than_pseudoclass
    assert_extends '.foo::bar', '.baz:bang {@extend .foo}', '.foo::bar, .baz:bang::bar'
    assert_extends '.foo:bar', '.baz::bang {@extend .foo}', '.foo:bar, .baz:bar::bang'
  end

  def test_pseudoelement_goes_lefter_than_not
    assert_extends '.foo::bar', '.baz:not(.bang) {@extend .foo}', '.foo::bar, .baz:not(.bang)::bar'
    assert_extends '.foo:not(.bang)', '.baz::bar {@extend .foo}', '.foo:not(.bang), .baz:not(.bang)::bar'
  end

  def test_negation_unification
    assert_extends ':not(.foo).baz', ':not(.bar) {@extend .baz}', ':not(.foo).baz, :not(.foo):not(.bar)'
    # Unifying to :not(.foo) here would reduce the specificity of the original selector.
    assert_extends ':not(.foo).baz', ':not(.foo) {@extend .baz}', ':not(.foo).baz, :not(.foo)'
  end

  def test_prefixed_pseudoclass_unification
    assert_unification(
      ':nth-child(2n+1 of .foo).baz',
      ':nth-child(2n of .foo) {@extend .baz}',
      ':nth-child(2n+1 of .foo).baz, :nth-child(2n+1 of .foo):nth-child(2n of .foo)')

    assert_unification(
      ':nth-child(2n+1 of .foo).baz',
      ':nth-child(2n+1 of .bar) {@extend .baz}',
      ':nth-child(2n+1 of .foo).baz, :nth-child(2n+1 of .foo):nth-child(2n+1 of .bar)')

    assert_unification(
      ':nth-child(2n+1 of .foo).baz',
      ':nth-child(2n+1 of .foo) {@extend .baz}',
      ':nth-child(2n+1 of .foo)')
  end

  def test_extend_into_not
    assert_extends(':not(.foo)', '.x {@extend .foo}', ':not(.foo):not(.x)')
    assert_extends(':not(.foo.bar)', '.x {@extend .bar}', ':not(.foo.bar):not(.foo.x)')
    assert_extends(
      ':not(.foo.bar, .baz.bar)',
      '.x {@extend .bar}',
      ':not(.foo.bar, .foo.x, .baz.bar, .baz.x)')
  end

  def test_extend_into_mergeable_pseudoclasses
    assert_extends(':matches(.foo)', '.x {@extend .foo}', ':matches(.foo, .x)')
    assert_extends(':matches(.foo.bar)', '.x {@extend .bar}', ':matches(.foo.bar, .foo.x)')
    assert_extends(
      ':matches(.foo.bar, .baz.bar)',
      '.x {@extend .bar}',
      ':matches(.foo.bar, .foo.x, .baz.bar, .baz.x)')

    assert_extends(':-moz-any(.foo)', '.x {@extend .foo}', ':-moz-any(.foo, .x)')
    assert_extends(':current(.foo)', '.x {@extend .foo}', ':current(.foo, .x)')
    assert_extends(':has(.foo)', '.x {@extend .foo}', ':has(.foo, .x)')
    assert_extends(':host(.foo)', '.x {@extend .foo}', ':host(.foo, .x)')
    assert_extends(':host-context(.foo)', '.x {@extend .foo}', ':host-context(.foo, .x)')
    assert_extends(':nth-child(n of .foo)', '.x {@extend .foo}', ':nth-child(n of .foo, .x)')
    assert_extends(
      ':nth-last-child(n of .foo)',
      '.x {@extend .foo}',
      ':nth-last-child(n of .foo, .x)')
  end

  def test_complex_extend_into_pseudoclass
    # Unlike other selectors, we don't allow complex selectors to be
    # added to `:not` if they weren't there before. At time of
    # writing, most browsers don't support that and will throw away
    # the entire selector if it exists.
    #assert_extends(':not(.bar)', '.x .y {@extend .bar}', ':not(.bar)')

    # If the `:not()` already has a complex selector, we won't break
    # anything by adding a new one.
    assert_extends(':not(.baz .bar)', '.x .y {@extend .bar}',
      ':not(.baz .bar):not(.baz .x .y):not(.x .baz .y)')

    # If the `:not()` would only contain complex selectors, there's no
    # harm in letting it continue to exist.
    assert_extends(':not(%bar)', '.x .y {@extend %bar}', ':not(.x .y)')

    assert_extends(':matches(.bar)', '.x .y {@extend .bar}', ':matches(.bar, .x .y)')
    assert_extends(':current(.bar)', '.x .y {@extend .bar}', ':current(.bar, .x .y)')
    assert_extends(':has(.bar)', '.x .y {@extend .bar}', ':has(.bar, .x .y)')
    assert_extends(':host(.bar)', '.x .y {@extend .bar}', ':host(.bar, .x .y)')
    assert_extends(':host-context(.bar)', '.x .y {@extend .bar}', ':host-context(.bar, .x .y)')
    assert_extends(
      ':-moz-any(.bar)',
      '.x .y {@extend .bar}',
      ':-moz-any(.bar, .x .y)')
    assert_extends(
      ':nth-child(n of .bar)',
      '.x .y {@extend .bar}',
      ':nth-child(n of .bar, .x .y)')
    assert_extends(
      ':nth-last-child(n of .bar)',
      '.x .y {@extend .bar}',
      ':nth-last-child(n of .bar, .x .y)')
  end

  def test_extend_over_selector_pseudoclass
    assert_extends(':not(.foo)', '.x {@extend :not(.foo)}', ':not(.foo), .x')
    assert_extends(
      ':matches(.foo, .bar)',
      '.x {@extend :matches(.foo, .bar)}',
      ':matches(.foo, .bar), .x')
  end

  def test_matches_within_not
    assert_extends(
      ':not(.foo, .bar)',
      ':matches(.x, .y) {@extend .foo}',
      ':not(.foo, .x, .y, .bar)')
  end

  def test_pseudoclasses_merge
    assert_extends(':matches(.foo)', ':matches(.bar) {@extend .foo}', ':matches(.foo, .bar)')
    assert_extends(':-moz-any(.foo)', ':-moz-any(.bar) {@extend .foo}', ':-moz-any(.foo, .bar)')
    assert_extends(':current(.foo)', ':current(.bar) {@extend .foo}', ':current(.foo, .bar)')
    assert_extends(
      ':nth-child(n of .foo)',
      ':nth-child(n of .bar) {@extend .foo}',
      ':nth-child(n of .foo, .bar)')
    assert_extends(
      ':nth-last-child(n of .foo)',
      ':nth-last-child(n of .bar) {@extend .foo}',
      ':nth-last-child(n of .foo, .bar)')
  end

  def test_nesting_pseudoclasses_merge
    assert_extends(':has(.foo)', ':has(.bar) {@extend .foo}', ':has(.foo, :has(.bar))')
    assert_extends(':host(.foo)', ':host(.bar) {@extend .foo}', ':host(.foo, :host(.bar))')
    assert_extends(
      ':host-context(.foo)',
      ':host-context(.bar) {@extend .foo}',
      ':host-context(.foo, :host-context(.bar))')
  end

  def test_not_unifies_with_unique_values
    assert_unification('foo', ':not(bar) {@extend foo}', ':not(bar)')
    assert_unification('#foo', ':not(#bar) {@extend #foo}', ':not(#bar)')
  end

  def test_not_adds_no_specificity
    assert_specificity_equals(':not(.foo)', '.foo')
  end

  def test_matches_has_a_specificity_range
    # `:matches(.foo, #bar)` has minimum specificity equal to that of `.foo`,
    # which means `:matches(.foo, #bar) .a` can have less specificity than
    # `#b.a`. Thus the selector generated by `#b.a` should be preserved.
    assert_equal <<CSS, render(<<SCSS)
:matches(.foo, #bar) .a, :matches(.foo, #bar) #b.a {
  a: b; }
CSS
:matches(.foo, #bar) %x {a: b}
.a {@extend %x}
#b.a {@extend %x}
SCSS

    # `:matches(.foo, #bar)` has maximum specificity equal to that of `#bar`,
    # which means `:matches(.foo, #bar).b` can have greater specificity than `.a
    # .b`. Thus the selector generated by `:matches(.foo, #bar).b` should be
    # preserved.
    assert_equal <<CSS, render(<<SCSS)
.a .b, .a .b:matches(.foo, #bar) {
  a: b; }
CSS
.a %x {a: b}
.b {@extend %x}
.b:matches(.foo, #bar) {@extend %x}
SCSS
  end

  def test_extend_into_not_and_normal_extend
    assert_equal <<CSS, render(<<SCSS)
.x:not(.y):not(.bar), .foo:not(.y):not(.bar) {
  a: b; }
CSS
.x:not(.y) {a: b}
.foo {@extend .x}
.bar {@extend .y}
SCSS
  end

  def test_extend_into_matches_and_normal_extend
    assert_equal <<CSS, render(<<SCSS)
.x:matches(.y, .bar), .foo:matches(.y, .bar) {
  a: b; }
CSS
.x:matches(.y) {a: b}
.foo {@extend .x}
.bar {@extend .y}
SCSS
  end

  def test_multilayer_pseudoclass_extend
    assert_equal <<CSS, render(<<SCSS)
:matches(.x, .foo, .bar) {
  a: b; }
CSS
:matches(.x) {a: b}
.foo {@extend .x}
.bar {@extend .foo}
SCSS
  end

  def test_root_only_allowed_at_root
    assert_extends(':root .foo', '.bar .baz {@extend .foo}',
      ':root .foo, :root .bar .baz')
    assert_extends('.foo:root .bar', '.baz:root .bang {@extend .bar}',
      '.foo:root .bar, .baz.foo:root .bang')
    assert_extends('html:root .bar', 'xml:root .bang {@extend .bar}', 'html:root .bar')
    assert_extends('.foo:root > .bar .x', '.baz:root .bang .y {@extend .x}',
      '.foo:root > .bar .x, .baz.foo:root > .bar .bang .y')
  end

  def test_comma_extendee
    assert_equal <<CSS, render(<<SCSS)
.foo, .baz {
  a: b; }

.bar, .baz {
  c: d; }
CSS
.foo {a: b}
.bar {c: d}
.baz {@extend .foo, .bar}
SCSS
  end

  def test_redundant_selector_elimination
    assert_equal <<CSS, render(<<SCSS)
.foo.bar, .x, .y {
  a: b; }
CSS
.foo.bar {a: b}
.x {@extend .foo, .bar}
.y {@extend .foo, .bar}
SCSS
  end

  def test_nested_pseudo_selectors
    assert_equal <<CSS, render(<<SCSS)
.foo .bar:not(.baz), .bang .bar:not(.baz) {
  a: b; }
CSS
.foo {
  .bar:not(.baz) {a: b}
}
.bang {@extend .foo}
SCSS
  end

  ## Long Extendees

  def test_long_extendee
    assert_extends '.foo.bar', '.baz {@extend .foo.bar}', '.foo.bar, .baz'
  end

  def test_long_extendee_requires_all_selectors
    assert_extend_doesnt_match('.baz', '.foo.bar', :not_found, 2) do
      render_extends '.foo', '.baz {@extend .foo.bar}'
    end
  end

  def test_long_extendee_matches_supersets
    assert_extends '.foo.bar.bap', '.baz {@extend .foo.bar}', '.foo.bar.bap, .bap.baz'
  end

  def test_long_extendee_runs_unification
    assert_extends 'ns|*.foo.bar', 'a.baz {@extend .foo.bar}', 'ns|*.foo.bar, ns|a.baz'
  end

  ## Long Extenders

  def test_long_extender
    assert_extends '.foo.bar', '.baz.bang {@extend .foo}', '.foo.bar, .bar.baz.bang'
  end

  def test_long_extender_runs_unification
    assert_extends 'ns|*.foo.bar', 'a.baz {@extend .foo}', 'ns|*.foo.bar, ns|a.bar.baz'
  end

  def test_long_extender_aborts_unification
    assert_extend_doesnt_match('h1.baz', '.foo', :failed_to_unify, 2) do
      render_extends 'a.foo#bar', 'h1.baz {@extend .foo}'
    end

    assert_extend_doesnt_match('.bang#baz', '.foo', :failed_to_unify, 2) do
      render_extends 'a.foo#bar', '.bang#baz {@extend .foo}'
    end
  end

  ## Nested Extenders

  def test_nested_extender
    assert_extends '.foo', 'foo bar {@extend .foo}', '.foo, foo bar'
  end

  def test_nested_extender_runs_unification
    assert_extends '.foo.bar', 'foo bar {@extend .foo}', '.foo.bar, foo bar.bar'
  end

  def test_nested_extender_aborts_unification
    assert_extend_doesnt_match('foo bar', '.foo', :failed_to_unify, 2) do
      render_extends 'baz.foo', 'foo bar {@extend .foo}'
    end
  end

  def test_nested_extender_alternates_parents
    assert_extends('.baz .bip .foo', 'foo .grank bar {@extend .foo}',
      '.baz .bip .foo, .baz .bip foo .grank bar, foo .grank .baz .bip bar')
  end

  def test_nested_extender_unifies_identical_parents
    assert_extends('.baz .bip .foo', '.baz .bip bar {@extend .foo}',
      '.baz .bip .foo, .baz .bip bar')
  end

  def test_nested_extender_unifies_common_substring
    assert_extends('.baz .bip .bap .bink .foo', '.brat .bip .bap bar {@extend .foo}',
      '.baz .bip .bap .bink .foo, .baz .brat .bip .bap .bink bar, .brat .baz .bip .bap .bink bar')
  end

  def test_nested_extender_unifies_common_subseq
    assert_extends('.a .x .b .y .foo', '.a .n .b .m bar {@extend .foo}',
      '.a .x .b .y .foo, .a .x .n .b .y .m bar, .a .n .x .b .y .m bar, .a .x .n .b .m .y bar, .a .n .x .b .m .y bar')
  end

  def test_nested_extender_chooses_first_subseq
    assert_extends('.a .b .c .d .foo', '.c .d .a .b .bar {@extend .foo}',
      '.a .b .c .d .foo, .a .b .c .d .a .b .bar')
  end

  def test_nested_extender_counts_extended_subselectors
    assert_extends('.a .bip.bop .foo', '.b .bip .bar {@extend .foo}',
      '.a .bip.bop .foo, .a .b .bip.bop .bar, .b .a .bip.bop .bar')
  end

  def test_nested_extender_counts_extended_superselectors
    assert_extends('.a .bip .foo', '.b .bip.bop .bar {@extend .foo}',
      '.a .bip .foo, .a .b .bip.bop .bar, .b .a .bip.bop .bar')
  end

  def test_nested_extender_with_child_selector
    assert_extends '.baz .foo', 'foo > bar {@extend .foo}', '.baz .foo, .baz foo > bar'
  end

  def test_nested_extender_finds_common_selectors_around_child_selector
    assert_extends 'a > b c .c1', 'a c .c2 {@extend .c1}', 'a > b c .c1, a > b c .c2'
    assert_extends 'a > b c .c1', 'b c .c2 {@extend .c1}', 'a > b c .c1, a > b c .c2'
  end

  def test_nested_extender_doesnt_find_common_selectors_around_adjacent_sibling_selector
    assert_extends 'a + b c .c1', 'a c .c2 {@extend .c1}', 'a + b c .c1, a + b a c .c2, a a + b c .c2'
    assert_extends 'a + b c .c1', 'a b .c2 {@extend .c1}', 'a + b c .c1, a a + b c .c2'
    assert_extends 'a + b c .c1', 'b c .c2 {@extend .c1}', 'a + b c .c1, a + b c .c2'
  end

  def test_nested_extender_doesnt_find_common_selectors_around_sibling_selector
    assert_extends 'a ~ b c .c1', 'a c .c2 {@extend .c1}', 'a ~ b c .c1, a ~ b a c .c2, a a ~ b c .c2'
    assert_extends 'a ~ b c .c1', 'a b .c2 {@extend .c1}', 'a ~ b c .c1, a a ~ b c .c2'
    assert_extends 'a ~ b c .c1', 'b c .c2 {@extend .c1}', 'a ~ b c .c1, a ~ b c .c2'
  end

  def test_nested_extender_doesnt_find_common_selectors_around_reference_selector
    assert_extends 'a /for/ b c .c1', 'a c .c2 {@extend .c1}', 'a /for/ b c .c1, a /for/ b a c .c2, a a /for/ b c .c2'
    assert_extends 'a /for/ b c .c1', 'a b .c2 {@extend .c1}', 'a /for/ b c .c1, a a /for/ b c .c2'
    assert_extends 'a /for/ b c .c1', 'b c .c2 {@extend .c1}', 'a /for/ b c .c1, a /for/ b c .c2'
  end

  def test_nested_extender_with_early_child_selectors_doesnt_subseq_them
    assert_extends('.bip > .bap .foo', '.grip > .bap .bar {@extend .foo}',
      '.bip > .bap .foo, .bip > .bap .grip > .bap .bar, .grip > .bap .bip > .bap .bar')
    assert_extends('.bap > .bip .foo', '.bap > .grip .bar {@extend .foo}',
      '.bap > .bip .foo, .bap > .bip .bap > .grip .bar, .bap > .grip .bap > .bip .bar')
  end

  def test_nested_extender_with_child_selector_unifies
    assert_extends '.baz.foo', 'foo > bar {@extend .foo}', '.baz.foo, foo > bar.baz'

    assert_equal <<CSS, render(<<SCSS)
.baz > .foo, .baz > .bar {
  a: b; }
CSS
.baz > {
  .foo {a: b}
  .bar {@extend .foo}
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo .bar, .foo > .baz {
  a: b; }
CSS
.foo {
  .bar {a: b}
  > .baz {@extend .bar}
}
SCSS
  end

  def test_nested_extender_with_early_child_selector
    assert_equal <<CSS, render(<<SCSS)
.foo .bar, .foo .bip > .baz {
  a: b; }
CSS
.foo {
  .bar {a: b}
  .bip > .baz {@extend .bar}
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo .bip .bar, .foo .bip .foo > .baz {
  a: b; }
CSS
.foo {
  .bip .bar {a: b}
  > .baz {@extend .bar}
}
SCSS

    assert_extends '.foo > .bar', '.bip + .baz {@extend .bar}', '.foo > .bar, .foo > .bip + .baz'
    assert_extends '.foo + .bar', '.bip > .baz {@extend .bar}', '.foo + .bar, .bip > .foo + .baz'
    assert_extends '.foo > .bar', '.bip > .baz {@extend .bar}', '.foo > .bar, .bip.foo > .baz'
  end

  def test_nested_extender_with_trailing_child_selector
    assert_raises(Sass::SyntaxError, "bar > can't extend: invalid selector") do
      render("bar > {@extend .baz}")
    end
  end

  def test_nested_extender_with_sibling_selector
    assert_extends '.baz .foo', 'foo + bar {@extend .foo}', '.baz .foo, .baz foo + bar'
  end

  def test_nested_extender_with_hacky_selector
    assert_extends('.baz .foo', 'foo + > > + bar {@extend .foo}',
      '.baz .foo, .baz foo + > > + bar, foo .baz + > > + bar')
    assert_extends '.baz .foo', '> > bar {@extend .foo}', '.baz .foo, > > .baz bar'
  end

  def test_nested_extender_merges_with_same_selector
    assert_equal <<CSS, render(<<SCSS)
.foo .bar, .foo .baz {
  a: b; }
CSS
.foo {
  .bar {a: b}
  .baz {@extend .bar} }
SCSS
  end

  def test_nested_extender_with_child_selector_merges_with_same_selector
    assert_extends('.foo > .bar .baz', '.foo > .bar .bang {@extend .baz}',
      '.foo > .bar .baz, .foo > .bar .bang')
  end

  # Combinator Unification

  def test_combinator_unification_for_hacky_combinators
    assert_extends '.a > + x', '.b y {@extend x}', '.a > + x, .a .b > + y, .b .a > + y'
    assert_extends '.a x', '.b > + y {@extend x}', '.a x, .a .b > + y, .b .a > + y'
    assert_extends '.a > + x', '.b > + y {@extend x}', '.a > + x, .a .b > + y, .b .a > + y'
    assert_extends '.a ~ > + x', '.b > + y {@extend x}', '.a ~ > + x, .a .b ~ > + y, .b .a ~ > + y'
    assert_extends '.a + > x', '.b > + y {@extend x}', '.a + > x'
    assert_extends '.a + > x', '.b > + y {@extend x}', '.a + > x'
    assert_extends '.a ~ > + .b > x', '.c > + .d > y {@extend x}', '.a ~ > + .b > x, .a .c ~ > + .d.b > y, .c .a ~ > + .d.b > y'
  end

  def test_combinator_unification_double_tilde
    assert_extends '.a.b ~ x', '.a ~ y {@extend x}', '.a.b ~ x, .a.b ~ y'
    assert_extends '.a ~ x', '.a.b ~ y {@extend x}', '.a ~ x, .a.b ~ y'
    assert_extends '.a ~ x', '.b ~ y {@extend x}', '.a ~ x, .a ~ .b ~ y, .b ~ .a ~ y, .b.a ~ y'
    assert_extends 'a.a ~ x', 'b.b ~ y {@extend x}', 'a.a ~ x, a.a ~ b.b ~ y, b.b ~ a.a ~ y'
  end

  def test_combinator_unification_tilde_plus
    assert_extends '.a.b + x', '.a ~ y {@extend x}', '.a.b + x, .a.b + y'
    assert_extends '.a + x', '.a.b ~ y {@extend x}', '.a + x, .a.b ~ .a + y, .a.b + y'
    assert_extends '.a + x', '.b ~ y {@extend x}', '.a + x, .b ~ .a + y, .b.a + y'
    assert_extends 'a.a + x', 'b.b ~ y {@extend x}', 'a.a + x, b.b ~ a.a + y'
    assert_extends '.a.b ~ x', '.a + y {@extend x}', '.a.b ~ x, .a.b ~ .a + y, .a.b + y'
    assert_extends '.a ~ x', '.a.b + y {@extend x}', '.a ~ x, .a.b + y'
    assert_extends '.a ~ x', '.b + y {@extend x}', '.a ~ x, .a ~ .b + y, .a.b + y'
    assert_extends 'a.a ~ x', 'b.b + y {@extend x}', 'a.a ~ x, a.a ~ b.b + y'
  end

  def test_combinator_unification_angle_sibling
    assert_extends '.a > x', '.b ~ y {@extend x}', '.a > x, .a > .b ~ y'
    assert_extends '.a > x', '.b + y {@extend x}', '.a > x, .a > .b + y'
    assert_extends '.a ~ x', '.b > y {@extend x}', '.a ~ x, .b > .a ~ y'
    assert_extends '.a + x', '.b > y {@extend x}', '.a + x, .b > .a + y'
  end

  def test_combinator_unification_double_angle
    assert_extends '.a.b > x', '.b > y {@extend x}', '.a.b > x, .b.a > y'
    assert_extends '.a > x', '.a.b > y {@extend x}', '.a > x, .a.b > y'
    assert_extends '.a > x', '.b > y {@extend x}', '.a > x, .b.a > y'
    assert_extends 'a.a > x', 'b.b > y {@extend x}', 'a.a > x'
  end

  def test_combinator_unification_double_plus
    assert_extends '.a.b + x', '.b + y {@extend x}', '.a.b + x, .b.a + y'
    assert_extends '.a + x', '.a.b + y {@extend x}', '.a + x, .a.b + y'
    assert_extends '.a + x', '.b + y {@extend x}', '.a + x, .b.a + y'
    assert_extends 'a.a + x', 'b.b + y {@extend x}', 'a.a + x'
  end

  def test_combinator_unification_angle_space
    assert_extends '.a.b > x', '.a y {@extend x}', '.a.b > x, .a.b > y'
    assert_extends '.a > x', '.a.b y {@extend x}', '.a > x, .a.b .a > y'
    assert_extends '.a > x', '.b y {@extend x}', '.a > x, .b .a > y'
    assert_extends '.a.b x', '.a > y {@extend x}', '.a.b x, .a.b .a > y'
    assert_extends '.a x', '.a.b > y {@extend x}', '.a x, .a.b > y'
    assert_extends '.a x', '.b > y {@extend x}', '.a x, .a .b > y'
  end

  def test_combinator_unification_plus_space
    assert_extends '.a.b + x', '.a y {@extend x}', '.a.b + x, .a .a.b + y'
    assert_extends '.a + x', '.a.b y {@extend x}', '.a + x, .a.b .a + y'
    assert_extends '.a + x', '.b y {@extend x}', '.a + x, .b .a + y'
    assert_extends '.a.b x', '.a + y {@extend x}', '.a.b x, .a.b .a + y'
    assert_extends '.a x', '.a.b + y {@extend x}', '.a x, .a .a.b + y'
    assert_extends '.a x', '.b + y {@extend x}', '.a x, .a .b + y'
  end

  def test_combinator_unification_nested
    assert_extends '.a > .b + x', '.c > .d + y {@extend x}', '.a > .b + x, .c.a > .d.b + y'
    assert_extends '.a > .b + x', '.c > y {@extend x}', '.a > .b + x, .c.a > .b + y'
  end

  def test_combinator_unification_with_newlines
    assert_equal <<CSS, render(<<SCSS)
.a >
.b
+ x, .c.a > .d.b + y {
  a: b; }
CSS
.a >
.b
+ x {a: b}
.c
> .d +
y {@extend x}
SCSS
  end

  # Loops

  def test_extend_self_loop
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: b; }
CSS
.foo {a: b; @extend .foo}
SCSS
  end

  def test_basic_extend_loop
    assert_equal <<CSS, render(<<SCSS)
.foo, .bar {
  a: b; }

.bar, .foo {
  c: d; }
CSS
.foo {a: b; @extend .bar}
.bar {c: d; @extend .foo}
SCSS
  end

  def test_three_level_extend_loop
    assert_equal <<CSS, render(<<SCSS)
.foo, .baz, .bar {
  a: b; }

.bar, .foo, .baz {
  c: d; }

.baz, .bar, .foo {
  e: f; }
CSS
.foo {a: b; @extend .bar}
.bar {c: d; @extend .baz}
.baz {e: f; @extend .foo}
SCSS
  end

  def test_nested_extend_loop
    assert_equal <<CSS, render(<<SCSS)
.bar, .bar .foo {
  a: b; }
  .bar .foo {
    c: d; }
CSS
.bar {
  a: b;
  .foo {c: d; @extend .bar}
}
SCSS
  end

  def test_cross_loop
    # The first law of extend means the selector should stick around.
    assert_equal <<CSS, render(<<SCSS)
.foo.bar, .foo, .bar {
  a: b; }
CSS
.foo.bar {a: b}
.foo {@extend .bar}
.bar {@extend .foo}
SCSS
  end

  def test_multiple_extender_merges_with_superset_selector
    assert_equal <<CSS, render(<<SCSS)
a.bar.baz, a.foo {
  a: b; }
CSS
.foo {@extend .bar; @extend .baz}
a.bar.baz {a: b}
SCSS
  end

  def test_control_flow_if
    assert_equal <<CSS, render(<<SCSS)
.true, .also-true {
  color: green; }

.false, .also-false {
  color: red; }
CSS
.true  { color: green; }
.false { color: red;   }
.also-true {
  @if true { @extend .true;  }
  @else    { @extend .false; }
}
.also-false {
  @if false { @extend .true;  }
  @else     { @extend .false; }
}
SCSS
  end

  def test_control_flow_for
    assert_equal <<CSS, render(<<SCSS)
.base-0, .added {
  color: green; }

.base-1, .added {
  display: block; }

.base-2, .added {
  border: 1px solid blue; }
CSS
.base-0  { color: green; }
.base-1  { display: block; }
.base-2  { border: 1px solid blue; }
.added {
  @for $i from 0 to 3 {
    @extend .base-\#{$i};
  }
}
SCSS
  end

  def test_control_flow_while
    assert_equal <<CSS, render(<<SCSS)
.base-0, .added {
  color: green; }

.base-1, .added {
  display: block; }

.base-2, .added {
  border: 1px solid blue; }
CSS
.base-0  { color: green; }
.base-1  { display: block; }
.base-2  { border: 1px solid blue; }
.added {
  $i : 0;
  @while $i < 3 {
    @extend .base-\#{$i};
    $i : $i + 1;
  }
}
SCSS
  end

  def test_basic_placeholder_selector
    assert_extends '%foo', '.bar {@extend %foo}', '.bar'
  end

  def test_unused_placeholder_selector
    assert_equal <<CSS, render(<<SCSS)
.baz {
  color: blue; }
CSS
%foo {color: blue}
%bar {color: red}
.baz {@extend %foo}
SCSS
  end

  def test_placeholder_descendant_selector
    assert_extends '#context %foo a', '.bar {@extend %foo}', '#context .bar a'
  end

  def test_semi_placeholder_selector
    assert_equal <<CSS, render(<<SCSS)
.bar .baz {
  color: blue; }
CSS
#context %foo, .bar .baz {color: blue}
SCSS
  end

  def test_placeholder_selector_with_multiple_extenders
    assert_equal <<CSS, render(<<SCSS)
.bar, .baz {
  color: blue; }
CSS
%foo {color: blue}
.bar {@extend %foo}
.baz {@extend %foo}
SCSS
  end

  def test_placeholder_selector_as_modifier
    assert_extend_doesnt_match('div', '%foo', :failed_to_unify, 3) do
      render(<<SCSS)
a%foo.baz {color: blue}
.bar {@extend %foo}
div {@extend %foo}
SCSS
    end
  end

  def test_placeholder_interpolation
    assert_equal <<CSS, render(<<SCSS)
.bar {
  color: blue; }
CSS
$foo: foo;

%\#{$foo} {color: blue}
.bar {@extend %foo}
SCSS
  end

  def test_placeholder_in_selector_pseudoclass
    assert_equal <<CSS, render(<<SCSS)
:matches(.bar, .baz) {
  color: blue; }
CSS
:matches(%foo) {color: blue}
.bar {@extend %foo}
.baz {@extend %foo}
SCSS
  end

  def test_media_in_placeholder_selector
    assert_equal <<CSS, render(<<SCSS)
.baz {
  c: d; }
CSS
%foo {bar {@media screen {a: b}}}
.baz {c: d}
SCSS
  end

  def test_extend_out_of_media
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @media.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 3 of test_extend_out_of_media_inline.scss.
ERR
.foo {a: b}
@media screen {
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_out_of_unknown_directive
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @flooblehoof.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 3 of test_extend_out_of_unknown_directive_inline.scss.
ERR
.foo {a: b}
@flooblehoof {
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_out_of_nested_directives
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @flooblehoof.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 4 of test_extend_out_of_nested_directives_inline.scss.
ERR
@media screen {
  .foo {a: b}
  @flooblehoof {
    .bar {@extend .foo}
  }
}
SCSS
  end

  def test_extend_within_media
    assert_equal(<<CSS, render(<<SCSS))
@media screen {
  .foo, .bar {
    a: b; } }
CSS
@media screen {
  .foo {a: b}
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_within_unknown_directive
    assert_equal(<<CSS, render(<<SCSS))
@flooblehoof {
  .foo, .bar {
    a: b; } }
CSS
@flooblehoof {
  .foo {a: b}
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_within_nested_directives
    assert_equal(<<CSS, render(<<SCSS))
@media screen {
  @flooblehoof {
    .foo, .bar {
      a: b; } } }
CSS
@media screen {
  @flooblehoof {
    .foo {a: b}
    .bar {@extend .foo}
  }
}
SCSS
  end

  def test_extend_within_disparate_media
    assert_equal(<<CSS, render(<<SCSS))
@media screen {
  .foo, .bar {
    a: b; } }
CSS
@media screen {.foo {a: b}}
@media screen {.bar {@extend .foo}}
SCSS
  end

  def test_extend_within_disparate_unknown_directive
    assert_equal(<<CSS, render(<<SCSS))
@flooblehoof {
  .foo, .bar {
    a: b; } }
@flooblehoof {}
CSS
@flooblehoof {.foo {a: b}}
@flooblehoof {.bar {@extend .foo}}
SCSS
  end

  def test_extend_within_disparate_nested_directives
    assert_equal(<<CSS, render(<<SCSS))
@media screen {
  @flooblehoof {
    .foo, .bar {
      a: b; } } }
@media screen {
  @flooblehoof {} }
CSS
@media screen {@flooblehoof {.foo {a: b}}}
@media screen {@flooblehoof {.bar {@extend .foo}}}
SCSS
  end

  def test_extend_within_and_without_media
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @media.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 4 of test_extend_within_and_without_media_inline.scss.
ERR
.foo {a: b}
@media screen {
  .foo {c: d}
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_within_and_without_unknown_directive
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @flooblehoof.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 4 of test_extend_within_and_without_unknown_directive_inline.scss.
ERR
.foo {a: b}
@flooblehoof {
  .foo {c: d}
  .bar {@extend .foo}
}
SCSS
  end

  def test_extend_within_and_without_nested_directives
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
You may not @extend an outer selector from within @flooblehoof.
You may only @extend selectors within the same directive.
From "@extend .foo" on line 5 of test_extend_within_and_without_nested_directives_inline.scss.
ERR
@media screen {
  .foo {a: b}
  @flooblehoof {
    .foo {c: d}
    .bar {@extend .foo}
  }
}
SCSS
  end

  def test_extend_with_subject_transfers_subject_to_extender
    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
foo bar! baz, foo .bip .bap! baz, .bip foo .bap! baz {
  a: b; }
CSS
foo bar! baz {a: b}
.bip .bap {@extend bar}
SCSS

    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
foo.x bar.y! baz.z, foo.x .bip bar.bap! baz.z, .bip foo.x bar.bap! baz.z {
  a: b; }
CSS
foo.x bar.y! baz.z {a: b}
.bip .bap {@extend .y}
SCSS
  end

  def test_extend_with_subject_retains_subject_on_target
    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
.foo! .bar, .foo! .bip .bap, .bip .foo! .bap {
  a: b; }
CSS
.foo! .bar {a: b}
.bip .bap {@extend .bar}
SCSS
  end

  def test_extend_with_subject_transfers_subject_to_target
    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
a.foo .bar, .bip a.bap! .bar {
  a: b; }
CSS
a.foo .bar {a: b}
.bip .bap! {@extend .foo}
SCSS
  end

  def test_extend_with_subject_retains_subject_on_extender
    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
.foo .bar, .foo .bip! .bap, .bip! .foo .bap {
  a: b; }
CSS
.foo .bar {a: b}
.bip! .bap {@extend .bar}
SCSS
  end

  def test_extend_with_subject_fails_with_conflicting_subject
    silence_warnings {assert_equal(<<CSS, render(<<SCSS))}
x! .bar {
  a: b; }
CSS
x! .bar {a: b}
y! .bap {@extend .bar}
SCSS
  end

  def test_extend_warns_when_extendee_doesnt_exist
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
".foo" failed to @extend ".bar".
The selector ".bar" was not found.
Use "@extend .bar !optional" if the extend should be able to fail.
ERR
.foo {@extend .bar}
SCSS
  end

  def test_extend_warns_when_extension_fails
    assert_raise_message(Sass::SyntaxError, <<ERR) {render(<<SCSS)}
"b.foo" failed to @extend ".bar".
No selectors matching ".bar" could be unified with "b.foo".
Use "@extend .bar !optional" if the extend should be able to fail.
ERR
a.bar {a: b}
b.foo {@extend .bar}
SCSS
  end

  def test_extend_succeeds_when_one_extension_fails_but_others_dont
    assert_equal(<<CSS, render(<<SCSS))
a.bar {
  a: b; }

.bar, b.foo {
  c: d; }
CSS
a.bar {a: b}
.bar {c: d}
b.foo {@extend .bar}
SCSS
  end

  def test_optional_extend_succeeds_when_extendee_doesnt_exist
    assert_equal("", render(<<SCSS))
.foo {@extend .bar !optional}
SCSS
  end

  def test_optional_extend_succeeds_when_extension_fails
    assert_equal(<<CSS, render(<<SCSS))
a.bar {
  a: b; }
CSS
a.bar {a: b}
b.foo {@extend .bar !optional}
SCSS
  end

  # Regression Tests

  def test_extend_with_middle_pseudo
    assert_equal(<<CSS, render(<<SCSS))
.btn:active.focus, :active.focus:before {
  a: b; }
CSS
.btn:active.focus {a: b}
:before {@extend .btn}
SCSS
  end

  def test_extend_parent_selector_suffix
    assert_equal <<CSS, render(<<SCSS)
.a-b, .c {
  x: y; }
CSS
.a {&-b {x: y}}
.c {@extend .a-b}
SCSS
  end

  def test_pseudo_element_superselector
    # Pseudo-elements shouldn't be removed in superselector calculations.
    assert_equal <<CSS, render(<<SCSS)
a#bar, a#bar::fblthp {
  a: b; }
CSS
%x#bar {a: b} // Add an id to make the results have high specificity
%y, %y::fblthp {@extend %x}
a {@extend %y}
SCSS

    # Pseudo-classes can be removed when the second law allows.
    assert_equal <<CSS, render(<<SCSS)
a#bar {
  a: b; }
CSS
%x#bar {a: b}
%y, %y:fblthp {@extend %x}
a {@extend %y}
SCSS

    # A few pseudo-elements can be written as pseudo-elements for historical
    # reasons. See http://www.w3.org/TR/selectors4/#pseudo-elements.
    %w[first-line first-letter before after].each do |pseudo|
      assert_equal <<CSS, render(<<SCSS)
a#bar, a#bar:#{pseudo} {
  a: b; }
CSS
%x#bar {a: b}
%y, %y:#{pseudo} {@extend %x}
a {@extend %y}
SCSS
    end
  end

  def test_multiple_source_redundancy_elimination
    assert_equal <<CSS, render(<<SCSS)
.test-case, .test-case:active {
  color: red; }

.test-case:hover {
  color: green; }
CSS
%default-color {color: red}
%alt-color {color: green}

%default-style {
  @extend %default-color;
  &:hover {@extend %alt-color}
  &:active {@extend %default-color}
}

.test-case {@extend %default-style}
SCSS
  end

  def test_nested_sibling_extend
    assert_equal <<CSS, render(<<SCSS)
.parent .bar, .parent .foo {
  width: 2000px; }
CSS
.foo {@extend .bar}

.parent {
  .bar {
    width: 2000px;
  }
  .foo {
    @extend .bar
  }
}
SCSS
  end

  def test_parent_and_sibling_extend
    assert_equal <<CSS, render(<<SCSS)
.parent1 .parent2 .child1.child2, .parent2 .parent1 .child1.child2 {
  c: d; }
CSS
%foo %bar%baz {c: d}

.parent1 {
  @extend %foo;
  .child1 {@extend %bar}
}

.parent2 {
  @extend %foo;
  .child2 {@extend %baz}
}
SCSS
  end

  def test_nested_extend_specificity
    assert_equal <<CSS, render(<<SCSS)
a :b, a :b:c {
  a: b; }
CSS
%foo {a: b}

a {
  :b {@extend %foo}
  :b:c {@extend %foo}
}
SCSS
  end

  def test_nested_double_extend_optimization
    assert_equal <<CSS, render(<<SCSS)
.parent1 .child {
  a: b; }
CSS
%foo %bar {
  a: b;
}

.parent1 {
  @extend %foo;

  .child {
    @extend %bar;
  }
}

.parent2 {
  @extend %foo;
}
SCSS
  end

  def test_extend_in_double_nested_media_query
    assert_equal <<CSS, render(<<SCSS)
@media all and (orientation: landscape) {
  .bar {
    color: blue; } }
CSS
@media all {
  @media (orientation: landscape) {
    %foo {color: blue}
    .bar {@extend %foo}
  }
}
SCSS
  end

  def test_partially_failed_extend
    assert_no_warning {assert_equal(<<CSS, render(<<SCSS))}
.rc, test {
  color: white; }

.prices span.pill span.rc {
  color: red; }
CSS
test { @extend .rc; }
.rc {color: white;}
.prices span.pill span.rc {color: red;}
SCSS
  end

  def test_newline_near_combinator
    assert_equal <<CSS, render(<<SCSS)
.a +
.b x, .a +
.b .c y, .c .a +
.b y {
  a: b; }
CSS
.a +
.b x {a: b}
.c y {@extend x}
SCSS
  end

  def test_duplicated_selector_with_newlines
    assert_equal(<<CSS, render(<<SCSS))
.example-1-1,
.example-1-2,
.my-page-1 .my-module-1-1,
.example-1-3 {
  a: b; }
CSS
.example-1-1,
.example-1-2,
.example-1-3 {
  a: b;
}

.my-page-1 .my-module-1-1 {@extend .example-1-2}
SCSS
  end

  def test_nested_selector_with_child_selector_hack_extendee
    assert_extends '> .foo', 'foo bar {@extend .foo}', '> .foo, > foo bar'
  end

  def test_nested_selector_with_child_selector_hack_extender
    assert_extends '.foo .bar', '> foo bar {@extend .bar}', '.foo .bar, > .foo foo bar, > foo .foo bar'
  end

  def test_nested_selector_with_child_selector_hack_extender_and_extendee
    assert_extends '> .foo', '> foo bar {@extend .foo}', '> .foo, > foo bar'
  end

  def test_nested_selector_with_child_selector_hack_extender_and_sibling_selector_extendee
    assert_extends '~ .foo', '> foo bar {@extend .foo}', '~ .foo'
  end

  def test_nested_selector_with_child_selector_hack_extender_and_extendee_and_newline
    assert_equal <<CSS, render(<<SCSS)
> .foo, > flip,
> foo bar {
  a: b; }
CSS
> .foo {a: b}
flip,
> foo bar {@extend .foo}
SCSS
  end

  def test_extended_parent_and_child_redundancy_elimination
    assert_equal <<CSS, render(<<SCSS)
a b, d b, a c, d c {
  a: b; }
CSS
a {
  b {a: b}
  c {@extend b}
}
d {@extend a}
SCSS
  end

  def test_extend_redundancy_elimination_when_it_would_reduce_specificity
    assert_extends 'a', 'a.foo {@extend a}', 'a, a.foo'
  end

  def test_extend_redundancy_elimination_when_it_would_preserve_specificity
    assert_extends '.bar a', 'a.foo {@extend a}', '.bar a'
  end

  def test_extend_redundancy_elimination_never_eliminates_base_selector
    assert_extends 'a.foo', '.foo {@extend a}', 'a.foo, .foo'
  end

  def test_extend_cross_branch_redundancy_elimination
    assert_equal <<CSS, render(<<SCSS)
.a .c .d, .b .c .a .d {
  a: b; }
CSS
%x .c %y {a: b}
.a, .b {@extend %x}
.a .d {@extend %y}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.e .a .c .d, .a .c .e .d, .e .b .c .a .d, .b .c .a .e .d {
  a: b; }
CSS
.e %z {a: b}
%x .c %y {@extend %z}
.a, .b {@extend %x}
.a .d {@extend %y}
SCSS
  end

  private

  def assert_extend_doesnt_match(extender, target, reason, line, syntax = :scss)
    message = "\"#{extender}\" failed to @extend \"#{target}\"."
    reason = 
      if reason == :not_found
        "The selector \"#{target}\" was not found."
      else
        "No selectors matching \"#{target}\" could be unified with \"#{extender}\"."
      end

    assert_raise_message(Sass::SyntaxError, <<ERR) {yield}
#{message}
#{reason}
Use "@extend #{target} !optional" if the extend should be able to fail.
ERR
  end

  def assert_unification(selector, extension, unified, nested = true)
    # Do some trickery so the first law of extend doesn't get in our way.
    assert_extends(
      "%-a #{selector}",
      extension + " -a {@extend %-a}",
      unified.split(', ').map {|s| "-a #{s}"}.join(', '))
  end

  def assert_specificity_equals(sel1, sel2)
    assert_specificity_gte(sel1, sel2)
    assert_specificity_gte(sel2, sel1)
  end

  def assert_specificity_gte(sel1, sel2)
    assert_equal <<CSS, render(<<SCSS)
#{sel1} .a {
  a: b; }
CSS
#{sel1} %-a {a: b}
.a {@extend %-a}
#{sel2}.a {@extend %-a}
SCSS
  end

  def render_unification(selector, extension)
    render_extends(
      "%-a #{selector}",
      extension + " -a {@extend %-a}")
  end

  def assert_extends(selector, extension, result)
    assert_equal <<CSS, render_extends(selector, extension)
#{result} {
  a: b; }
CSS
  end

  def assert_extends_to_nothing(selector, extension)
    assert_equal '', render_extends(selector, extension)
  end

  def render_extends(selector, extension)
    render(<<SCSS)
#{selector} {a: b}
#{extension}
SCSS
  end

  def render(sass, options = {})
    options = {:syntax => :scss}.merge(options)
    munge_filename options
    Sass::Engine.new(sass, options).render
  end
end
