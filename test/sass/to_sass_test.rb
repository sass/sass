#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ToSassTest < Test::Unit::TestCase
  def test_basic
    assert_renders <<SASS, <<SCSS
foo bar
  baz: bang
  bip: bop
SASS
foo bar {
  baz: bang;
  bip: bop; }
SCSS
    assert_renders <<SASS, <<SCSS, :old => true
foo bar
  :baz bang
  :bip bop
SASS
foo bar {
  baz: bang;
  bip: bop; }
SCSS
  end

  def test_nesting
    assert_renders <<SASS, <<SCSS
foo bar
  baz bang
    baz: bang
    bip: bop
  blat: boo
SASS
foo bar {
  baz bang {
    baz: bang;
    bip: bop; }
  blat: boo; }
SCSS
  end

  def test_nesting_with_parent_ref
    assert_renders <<SASS, <<SCSS
foo bar
  &:hover
    baz: bang
SASS
foo bar {
  &:hover {
    baz: bang; } }
SCSS
  end

  def test_selector_interpolation
    assert_renders <<SASS, <<SCSS
foo \#{!bar + "baz"}.bip
  baz: bang
SASS
foo \#{!bar + "baz"}.bip {
  baz: bang; }
SCSS
  end

  def test_multiline_selector_with_commas
    assert_renders <<SASS, <<SCSS
foo bar,
baz bang
  baz: bang
SASS
foo bar,
baz bang {
  baz: bang; }
SCSS

    assert_renders <<SASS, <<SCSS
blat
  foo bar,
  baz bang
    baz: bang
SASS
blat {
  foo bar,
  baz bang {
    baz: bang; } }
SCSS
  end

  def test_multiline_selector_without_commas
    assert_renders <<SASS, <<SCSS
foo bar baz bang
  baz: bang
SASS
foo bar
baz bang {
  baz: bang; }
SCSS
  end

  def test_escaped_selector
    assert_renders <<SASS, <<SCSS
foo bar
  \\:hover
    baz: bang
SASS
foo bar {
  :hover {
    baz: bang; } }
SCSS
  end

  def test_property_name_interpolation
assert_renders <<SASS, <<SCSS
foo bar
  baz\#{!bang}bip\#{!bop}: 12
SASS
foo bar {
  baz\#{!bang}bip\#{!bop}: 12; }
SCSS
  end

  def test_property_name_interpolation
assert_renders <<SASS, <<SCSS
foo bar
  baz\#{!bang}bip\#{!bop}: 12
SASS
foo bar {
  baz\#{!bang}bip\#{!bop}: 12; }
SCSS
  end

  def test_property_value_interpolation
assert_renders <<SASS, <<SCSS
foo bar
  baz: 12 \#{!bang} bip \#{"bop"} blat
SASS
foo bar {
  baz: 12 \#{!bang} bip \#{"bop"} blat; }
SCSS
  end

  def test_dynamic_properties
assert_renders <<SASS, <<SCSS
foo bar
  baz= 12 !bang "bip"
SASS
foo bar {
  baz= 12 !bang "bip"; }
SCSS
  end

  def test_dynamic_properties_with_old
assert_renders <<SASS, <<SCSS, :old => true
foo bar
  :baz= 12 !bang "bip"
SASS
foo bar {
  baz= 12 !bang "bip"; }
SCSS
  end

  def test_multiline_properties
assert_renders <<SASS, <<SCSS
foo bar
  baz: bip bam boon
SASS
foo bar {
  baz:
    bip
  bam
        boon; }
SCSS
  end

  def test_multiline_dynamic_properties
assert_renders <<SASS, <<SCSS
foo bar
  baz= !bip "bam" 12px
SASS
foo bar {
  baz=
    !bip
  "bam"
        12px; }
SCSS
  end

  def test_silent_comments
assert_renders <<SASS, <<SCSS
// foo

// bar

// baz

foo bar
  a: b
SASS
// foo

// bar

// baz

foo bar {
  a: b; }
SCSS

assert_renders <<SASS, <<SCSS
// foo
   bar
     baz
   bang

foo bar
  a: b
SASS
// foo
// bar
//   baz
// bang

foo bar {
  a: b; }
SCSS
  end


  def test_loud_comments
assert_renders <<SASS, <<SCSS
/* foo

/* bar

/* baz

foo bar
  a: b
SASS
/* foo */

/* bar */

/* baz */

foo bar {
  a: b; }
SCSS

assert_renders <<SASS, <<SCSS
/* foo
   bar
     baz
   bang

foo bar
  a: b
SASS
/* foo
   bar
     baz
   bang */

foo bar {
  a: b; }
SCSS

assert_renders <<SASS, <<SCSS
/* foo
   bar
     baz
   bang

foo bar
  a: b
SASS
/* foo
 * bar
 *   baz
 * bang */

foo bar {
  a: b; }
SCSS
  end

  def test_loud_comments_with_weird_indentation
    assert_renders <<SASS, <<SCSS
foo
  /*      foo
     bar
         baz
  a: b
SASS
foo {
  /* foo
bar
    baz */
  a: b; }
SCSS
  end

  def test_debug
    assert_renders <<SASS, <<SCSS
foo
  @debug 12px
  bar: baz
SASS
foo {
  @debug 12px;
  bar: baz; }
SCSS
  end

  def test_directive_without_children
    assert_renders <<SASS, <<SCSS
foo
  @foo #bar "baz"
  bar: baz
SASS
foo {
  @foo #bar "baz";
  bar: baz; }
SCSS
  end

  def test_directive_with_prop_children
    assert_renders <<SASS, <<SCSS
foo
  @foo #bar "baz"
    a: b
    c: d

  bar: baz
SASS
foo {
  @foo #bar "baz" {
    a: b;
    c: d; }

  bar: baz; }
SCSS
  end

  def test_directive_with_rule_children
    assert_renders <<SASS, <<SCSS
foo
  @foo #bar "baz"
    #blat
      a: b
    .bang
      c: d
      e: f

  bar: baz
SASS
foo {
  @foo #bar "baz" {
    #blat {
      a: b; }
    .bang {
      c: d;
      e: f; } }

  bar: baz; }
SCSS
  end

  def test_directive_with_rule_and_prop_children
    assert_renders <<SASS, <<SCSS
foo
  @foo #bar "baz"
    g: h
    #blat
      a: b
    .bang
      c: d
      e: f
    i: j

  bar: baz
SASS
foo {
  @foo #bar "baz" {
    g: h;
    #blat {
      a: b; }
    .bang {
      c: d;
      e: f; }
    i: j; }

  bar: baz; }
SCSS
  end

  def test_for
    assert_renders <<SASS, <<SCSS
foo
  @for !a from !b to !c
    a: b
  @for !c from 1 to 16
    d: e
    f: g
SASS
foo {
  @for !a from !b to !c {
    a: b; }
  @for !c from 1 to 16 {
    d: e;
    f: g; } }
SCSS
  end

  def test_if
    assert_renders <<SASS, <<SCSS
foo
  @if !foo or !bar
    a: b
  @if !baz
    d: e
  @else if !bang
    f: g
  @else
    h: i
SASS
foo {
  @if !foo or !bar {
    a: b; }
  @if !baz {
    d: e; }
  @else if !bang {
    f: g; }
  @else {
    h: i; } }
SCSS
  end

  def test_import
    assert_renders <<SASS, <<SCSS
@import foo

foo
  bar: baz
SASS
@import "foo";

foo {
  bar: baz; }
SCSS

    assert_renders <<SASS, <<SCSS
@import foo.css

foo
  bar: baz
SASS
@import "foo.css";

foo {
  bar: baz; }
SCSS
  end

  def test_import_as_directive_in_sass
    assert_sass_to_sass '@import "foo.css"'
    assert_sass_to_sass '@import url(foo.css)'
  end

  def test_import_as_directive_in_scss
    assert_renders <<SASS, <<SCSS
@import "foo.css" print
SASS
@import "foo.css" print;
SCSS

    assert_renders <<SASS, <<SCSS
@import url(foo.css) screen, print
SASS
@import url(foo.css) screen, print;
SCSS
  end

  def test_argless_mixin_definition
    assert_renders <<SASS, <<SCSS
=foo-bar
  baz
    a: b
SASS
@mixin foo-bar {
  baz {
    a: b; } }
SCSS

    assert_renders <<SASS, render(<<SCSS)
=foo-bar
  baz
    a: b
SASS
@mixin foo-bar() {
  baz {
    a: b; } }
SCSS
  end

  def test_mixin_definition_without_defaults
    assert_renders <<SASS, <<SCSS
=foo-bar(!baz, !bang)
  baz
    a = !baz !bang
SASS
@mixin foo-bar(!baz, !bang) {
  baz {
    a = !baz !bang; } }
SCSS
  end

  def test_mixin_definition_with_defaults
    assert_renders <<SASS, <<SCSS
=foo-bar(!baz, !bang = 12px)
  baz
    a = !baz !bang
SASS
@mixin foo-bar(!baz, !bang = 12px) {
  baz {
    a = !baz !bang; } }
SCSS
  end

  private

  def assert_sass_to_sass(sass, options = {})
    assert_equal(sass.rstrip, render(sass, options).rstrip,
      "Expected Sass to transform to itself")
  end

  def assert_scss_to_sass(sass, scss, options = {})
    assert_equal(sass.rstrip, render(scss, options.merge(:syntax => :scss)).rstrip,
      "Expected SCSS to transform to Sass")
  end

  def assert_renders(sass, scss, options = {})
    assert_sass_to_sass(sass, options)
    assert_scss_to_sass(sass, scss, options)
  end

  def render(scss, options = {})
    Sass::Engine.new(scss, options).to_tree.to_sass(options)
  end
end
