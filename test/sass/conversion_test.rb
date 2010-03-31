#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ConversionTest < Test::Unit::TestCase
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
foo \#{$bar + "baz"}.bip
  baz: bang
SASS
foo \#{$bar + "baz"}.bip {
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
    assert_scss_to_sass <<SASS, <<SCSS
foo bar baz bang
  baz: bang
SASS
foo bar
baz bang {
  baz: bang; }
SCSS

    assert_scss_to_scss <<SCSS
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
  baz\#{$bang}bip\#{$bop}: 12
SASS
foo bar {
  baz\#{$bang}bip\#{$bop}: 12; }
SCSS
  end

  def test_property_name_interpolation
    assert_renders <<SASS, <<SCSS
foo bar
  baz\#{$bang}bip\#{$bop}: 12
SASS
foo bar {
  baz\#{$bang}bip\#{$bop}: 12; }
SCSS
  end

  def test_property_value_interpolation
    assert_renders <<SASS, <<SCSS
foo bar
  baz: 12 \#{$bang} bip \#{"bop"} blat
SASS
foo bar {
  baz: 12 \#{$bang} bip \#{"bop"} blat; }
SCSS
  end

  def test_dynamic_properties
    assert_renders <<SASS, <<SCSS
foo bar
  baz: 12 $bang "bip"
SASS
foo bar {
  baz: 12 $bang "bip"; }
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
foo bar {
  baz: 12 $bang bip; }
SCSS
foo bar
  baz= 12 $bang "bip"
SASS
  end

  def test_dynamic_properties_with_old
    assert_renders <<SASS, <<SCSS, :old => true
foo bar
  :baz 12 $bang "bip"
SASS
foo bar {
  baz: 12 $bang "bip"; }
SCSS

    assert_sass_to_scss <<SCSS, <<SASS, :old => true
foo bar {
  baz: 12 $bang bip; }
SCSS
foo bar
  :baz= 12 $bang "bip"
SASS
  end

  def test_multiline_properties
    assert_scss_to_sass <<SASS, <<SCSS
foo bar
  baz: bip bam boon
SASS
foo bar {
  baz:
    bip
  bam
        boon; }
SCSS

    assert_scss_to_scss <<OUT, <<IN
foo bar {
  baz: bip bam boon; }
OUT
foo bar {
  baz:
    bip
  bam
        boon; }
IN
  end

  def test_multiline_dynamic_properties
    assert_scss_to_sass <<SASS, <<SCSS
foo bar
  baz: $bip "bam" 12px
SASS
foo bar {
  baz:
    $bip
  "bam"
        12px; }
SCSS

    assert_scss_to_scss <<OUT, <<IN
foo bar {
  baz: $bip "bam" 12px; }
OUT
foo bar {
  baz:
    $bip
  "bam"
        12px; }
IN
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

    assert_sass_to_scss <<SCSS, <<SASS
// foo
// bar
//   baz
// bang

foo bar {
  a: b; }
SCSS
// foo
// bar
//   baz
// bang

foo bar
  a: b
SASS
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

    assert_scss_to_sass <<SASS, <<SCSS
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

    assert_scss_to_scss <<SCSS
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
    assert_scss_to_sass <<SASS, <<SCSS
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

    assert_sass_to_scss <<SCSS, <<SASS
foo {
  /*      foo
   * bar
   *     baz */
  a: b; }
SCSS
foo
  /*      foo
     bar
         baz
  a: b
SASS
  end

  def test_immediately_preceding_comments
    assert_renders <<SASS, <<SCSS
/* Foo
   Bar
   Baz
.foo#bar
  a: b
SASS
/* Foo
 * Bar
 * Baz */
.foo#bar {
  a: b; }
SCSS

    assert_renders <<SASS, <<SCSS
// Foo
   Bar
   Baz
=foo
  a: b
SASS
// Foo
// Bar
// Baz
@mixin foo {
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
  @for $a from $b to $c
    a: b
  @for $c from 1 to 16
    d: e
    f: g
SASS
foo {
  @for $a from $b to $c {
    a: b; }
  @for $c from 1 to 16 {
    d: e;
    f: g; } }
SCSS
  end

  def test_while
    assert_renders <<SASS, <<SCSS
foo
  @while flaz($a + $b)
    a: b
  @while 1
    d: e
    f: g
SASS
foo {
  @while flaz($a + $b) {
    a: b; }
  @while 1 {
    d: e;
    f: g; } }
SCSS
  end

  def test_if
    assert_renders <<SASS, <<SCSS
foo
  @if $foo or $bar
    a: b
  @if $baz
    d: e
  @else if $bang
    f: g
  @else
    h: i
SASS
foo {
  @if $foo or $bar {
    a: b; }
  @if $baz {
    d: e; }
  @else if $bang {
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

  def test_adjacent_imports
    assert_renders <<SASS, <<SCSS
@import foo.sass
@import bar.scss
@import baz
SASS
@import "foo.sass";
@import "bar.scss";
@import "baz";
SCSS
  end

  def test_non_adjacent_imports
    assert_renders <<SASS, <<SCSS
@import foo.sass

@import bar.scss

@import baz
SASS
@import "foo.sass";

@import "bar.scss";

@import "baz";
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

    assert_scss_to_sass <<SASS, <<SCSS
=foo-bar
  baz
    a: b
SASS
@mixin foo-bar() {
  baz {
    a: b; } }
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
@mixin foo-bar {
  baz {
    a: b; } }
SCSS
=foo-bar()
  baz
    a: b
SASS
  end

  def test_mixin_definition_without_defaults
    assert_renders <<SASS, <<SCSS
=foo-bar($baz, $bang)
  baz
    a: $baz $bang
SASS
@mixin foo-bar($baz, $bang) {
  baz {
    a: $baz $bang; } }
SCSS
  end

  def test_mixin_definition_with_defaults
    assert_renders <<SASS, <<SCSS
=foo-bar($baz, $bang: 12px)
  baz
    a: $baz $bang
SASS
@mixin foo-bar($baz, $bang: 12px) {
  baz {
    a: $baz $bang; } }
SCSS

    assert_scss_to_sass <<SASS, <<SCSS
=foo-bar($baz, $bang: foo)
  baz
    a: $baz $bang
SASS
@mixin foo-bar($baz, $bang = "foo") {
  baz {
    a: $baz $bang; } }
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
@mixin foo-bar($baz, $bang: foo) {
  baz {
    a: $baz $bang; } }
SCSS
=foo-bar($baz, $bang = "foo")
  baz
    a: $baz $bang
SASS
  end

  def test_argless_mixin_include
    assert_renders <<SASS, <<SCSS
foo
  +foo-bar
  a: blip
SASS
foo {
  @include foo-bar;
  a: blip; }
SCSS
  end

  def test_mixin_include
    assert_renders <<SASS, <<SCSS
foo
  +foo-bar(12px, "blaz")
  a: blip
SASS
foo {
  @include foo-bar(12px, "blaz");
  a: blip; }
SCSS
  end

  def test_variable_definition
    assert_renders <<SASS, <<SCSS
$var1: 12px + 15px

foo
  $var2: flaz(#abcdef)
  val: $var1 $var2
SASS
$var1: 12px + 15px;

foo {
  $var2: flaz(#abcdef);
  val: $var1 $var2; }
SCSS

    assert_sass_to_scss '$var: 12px $bar baz;', '$var = 12px $bar "baz"'
  end

  def test_guarded_variable_definition
    assert_renders <<SASS, <<SCSS
$var1: 12px + 15px !default

foo
  $var2: flaz(#abcdef) !default
  val: $var1 $var2
SASS
$var1: 12px + 15px !default;

foo {
  $var2: flaz(#abcdef) !default;
  val: $var1 $var2; }
SCSS

    assert_sass_to_scss '$var: 12px $bar baz !default;', '$var ||= 12px $bar "baz"'
  end

  # Sass 3 Deprecation conversions

  def test_simple_quoted_strings_unquoted_with_equals
    assert_sass_to_scss '$var: 1px foo + bar baz;', '!var = 1px "foo" + "bar" baz'
    assert_sass_to_scss '$var: -foo-bar;', '!var = "-foo-bar"'
  end

  def test_complex_quoted_strings_explicitly_unquoted_with_equals
    assert_sass_to_scss '$var: 1px unquote("foo + bar") baz;', '!var = 1px "foo + bar" baz'
    assert_sass_to_scss "$var: unquote('foo\"bar');", '!var = "foo\"bar"'
  end

  def test_division_asserted_with_equals
    assert_sass_to_scss <<SCSS, <<SASS
foo {
  a: (1px / 2px); }
SCSS
foo
  a = 1px / 2px
SASS
  end

  def test_division_not_asserted_with_equals_when_unnecessary
    assert_sass_to_scss <<SCSS, <<SASS
$var: 1px / 2px;

foo {
  a: $var; }
SCSS
!var = 1px / 2px

foo
  a = !var
SASS

    assert_sass_to_scss <<SCSS, <<SASS
$var: 1px;

foo {
  a: $var / 2px; }
SCSS
!var = 1px

foo
  a = !var / 2px
SASS

    assert_sass_to_scss <<SCSS, <<SASS
foo {
  a: 1 + 1px / 2px; }
SCSS
foo
  a = 1 + 1px / 2px
SASS
  end

  private

  def assert_sass_to_sass(sass, options = {})
    assert_equal(sass.rstrip, to_sass(sass, options).rstrip,
      "Expected Sass to transform to itself")
  end

  def assert_scss_to_sass(sass, scss, options = {})
    assert_equal(sass.rstrip, to_sass(scss, options.merge(:syntax => :scss)).rstrip,
      "Expected SCSS to transform to Sass")
  end

  def assert_scss_to_scss(scss, in_scss = nil, options = nil)
    if in_scss.is_a?(Hash)
      options = in_scss
      in_scss = nil
    end

    in_scss ||= scss
    options ||= {}

    assert_equal(scss.rstrip, to_scss(in_scss, options.merge(:syntax => :scss)).rstrip,
      "Expected SCSS to transform to #{scss == in_scss ? 'itself' : 'SCSS'}k")
  end

  def assert_sass_to_scss(scss, sass, options = {})
    assert_equal(scss.rstrip, to_scss(sass, options).rstrip,
      "Expected Sass to transform to SCSS")
  end

  def assert_renders(sass, scss, options = {})
    assert_sass_to_sass(sass, options)
    assert_scss_to_sass(sass, scss, options)
    assert_scss_to_scss(scss, options)
    assert_sass_to_scss(scss, sass, options)
  end

  def to_sass(scss, options = {})
    Haml::Util.silence_haml_warnings do
      Sass::Engine.new(scss, options).to_tree.to_sass(options)
    end
  end

  def to_scss(sass, options = {})
    Haml::Util.silence_haml_warnings do
      Sass::Engine.new(sass, options).to_tree.to_scss(options)
    end
  end
end
