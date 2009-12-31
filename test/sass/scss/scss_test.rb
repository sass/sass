#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/test_helper'

class ScssTest < Test::Unit::TestCase
  include ScssTestHelper

  ## Nested Rules

  def test_nested_rules
    assert_equal <<CSS, render(<<SCSS)
foo bar {
  a: b; }
CSS
foo {bar {a: b}}
SCSS
    assert_equal <<CSS, render(<<SCSS)
foo bar {
  a: b; }
foo baz {
  b: c; }
CSS
foo {
  bar {a: b}
  baz {b: c}}
SCSS
    assert_equal <<CSS, render(<<SCSS)
foo bar baz {
  a: b; }
foo bang bip {
  a: b; }
CSS
foo {
  bar {baz {a: b}}
  bang {bip {a: b}}}
SCSS
  end

  def test_nested_rules_with_declarations
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: b; }
  foo bar {
    c: d; }
CSS
foo {
  a: b;
  bar {c: d}}
SCSS
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: b; }
  foo bar {
    c: d; }
CSS
foo {
  bar {c: d}
  a: b}
SCSS
    assert_equal <<CSS, render(<<SCSS)
foo {
  ump: nump;
  grump: clump; }
  foo bar {
    blat: bang;
    habit: rabbit; }
    foo bar baz {
      a: b; }
    foo bar bip {
      c: d; }
  foo bibble bap {
    e: f; }
CSS
foo {
  ump: nump;
  grump: clump;
  bar {
    blat: bang;
    habit: rabbit;
    baz {a: b}
    bip {c: d}}
  bibble {
    bap {e: f}}}
SCSS
  end

  def test_nested_rules_with_fancy_selectors
    assert_equal <<CSS, render(<<SCSS)
foo .bar {
  a: b; }
foo :baz {
  c: d; }
foo bang:bop {
  e: f; }
CSS
foo {
  .bar {a: b}
  :baz {c: d}
  bang:bop {e: f}}
SCSS
  end

  def test_almost_ambiguous_nested_rules_and_declarations
    assert_equal <<CSS, render(<<SCSS)
foo {
  bar: baz:bang:bop:biddle:woo:look:at:all:these:pseudoclasses;
  bar: baz bang bop biddle woo look at all these elems; }
  foo bar:baz bang bop biddle woo look at all these elems {
    a: b; }
CSS
foo {
  bar:baz:bang:bop:biddle:woo:look:at:all:these:pseudoclasses;
  bar:baz bang bop biddle woo look at all these elems {a: b};
  bar:baz bang bop biddle woo look at all these elems; }
SCSS
  end

  def test_newlines_in_selectors
    assert_equal <<CSS, render(<<SCSS)
foo
bar {
  a: b; }
CSS
foo
bar {a: b}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo baz,
foo bang,
bar baz,
bar bang {
  a: b; }
CSS
foo,
bar {
  baz,
  bang {a: b}}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo
bar baz
bang {
  a: b; }
foo
bar bip bop {
  c: d; }
CSS
foo
bar {
  baz
  bang {a: b}

  bip bop {c: d}}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo bang, foo bip
bop, bar
baz bang, bar
baz bip
bop {
  a: b; }
CSS
foo, bar
baz {
  bang, bip
  bop {a: b}}
SCSS
  end

  def test_parent_selectors
    assert_equal <<CSS, render(<<SCSS)
foo:hover {
  a: b; }
bar foo.baz {
  c: d; }
CSS
foo {
  &:hover {a: b}
  bar &.baz {c: d}}
SCSS
  end
end
