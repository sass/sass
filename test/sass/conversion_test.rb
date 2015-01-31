#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ConversionTest < MiniTest::Test
  def test_basic
    assert_renders <<SASS, <<SCSS
foo bar
  baz: bang
  bip: bop
SASS
foo bar {
  baz: bang;
  bip: bop;
}
SCSS
    assert_renders <<SASS, <<SCSS, :old => true
foo bar
  :baz bang
  :bip bop
SASS
foo bar {
  baz: bang;
  bip: bop;
}
SCSS
  end

  def test_empty_selector
    assert_renders "foo bar", "foo bar {}"
  end

  def test_empty_directive
    assert_renders "@media screen", "@media screen {}"
  end

  def test_empty_control_directive
    assert_renders "@if false", "@if false {}"
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
    bip: bop;
  }
  blat: boo;
}
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
    baz: bang;
  }
}
SCSS
  end

  def test_selector_interpolation
    assert_renders <<SASS, <<SCSS
foo \#{$bar + "baz"}.bip
  baz: bang

foo /\#{$bar + "baz"}/ .bip
  baz: bang
SASS
foo \#{$bar + "baz"}.bip {
  baz: bang;
}

foo /\#{$bar + "baz"}/ .bip {
  baz: bang;
}
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
  baz: bang;
}
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
    baz: bang;
  }
}
SCSS
  end

  def test_multiline_selector_without_commas
    assert_scss_to_sass <<SASS, <<SCSS
foo bar baz bang
  baz: bang
SASS
foo bar
baz bang {
  baz: bang;
}
SCSS

    assert_scss_to_scss <<SCSS
foo bar
baz bang {
  baz: bang;
}
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
    baz: bang;
  }
}
SCSS
  end

  def test_property_name_interpolation
    assert_renders <<SASS, <<SCSS
foo bar
  baz\#{$bang}bip\#{$bop}: 12
SASS
foo bar {
  baz\#{$bang}bip\#{$bop}: 12;
}
SCSS
  end

  def test_property_value_interpolation
    assert_renders <<SASS, <<SCSS
foo bar
  baz: 12 \#{$bang} bip \#{"bop"} blat
SASS
foo bar {
  baz: 12 \#{$bang} bip \#{"bop"} blat;
}
SCSS
  end

  def test_dynamic_properties
    assert_renders <<SASS, <<SCSS
foo bar
  baz: 12 $bang "bip"
SASS
foo bar {
  baz: 12 $bang "bip";
}
SCSS
  end

  def test_dynamic_properties_with_old
    assert_renders <<SASS, <<SCSS, :old => true
foo bar
  :baz 12 $bang "bip"
SASS
foo bar {
  baz: 12 $bang "bip";
}
SCSS
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
        boon;
}
SCSS

    assert_scss_to_scss <<OUT, <<IN
foo bar {
  baz: bip bam boon;
}
OUT
foo bar {
  baz:
    bip
  bam
        boon;
}
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
        12px;
}
SCSS

    assert_scss_to_scss <<OUT, <<IN
foo bar {
  baz: $bip "bam" 12px;
}
OUT
foo bar {
  baz:
    $bip
  "bam"
        12px;
}
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
  a: b;
}
SCSS

    assert_renders <<SASS, <<SCSS
// foo
// bar
//   baz
// bang

foo bar
  a: b
SASS
// foo
// bar
//   baz
// bang

foo bar {
  a: b;
}
SCSS

    assert_renders <<SASS, <<SCSS
// foo
// bar
//   baz
// bang

foo bar
  a: b
SASS
// foo
// bar
//   baz
// bang

foo bar {
  a: b;
}
SCSS
  end

  def test_nested_silent_comments
    assert_renders <<SASS, <<SCSS
foo
  bar: baz
  // bip bop
  // beep boop
  bang: bizz
  // bubble bubble
  // toil trouble
SASS
foo {
  bar: baz;
  // bip bop
  // beep boop
  bang: bizz;
  // bubble bubble
  // toil trouble
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
foo {
  bar: baz;
  // bip bop
  // beep boop
  //   bap blimp
  bang: bizz;
  // bubble bubble
  // toil trouble
  //    gorp
}
SCSS
foo
  bar: baz
  // bip bop
     beep boop
       bap blimp
  bang: bizz
  // bubble bubble
    toil trouble
       gorp
SASS
  end

  def test_preserves_triple_slash_comments
    assert_renders <<SASS, <<SCSS
/// foo
/// bar
foo
  /// bip bop
  /// beep boop
SASS
/// foo
/// bar
foo {
  /// bip bop
  /// beep boop
}
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
  a: b;
}
SCSS

    assert_scss_to_sass <<SASS, <<SCSS
/* foo
 * bar
 *   baz
 * bang

foo bar
  a: b
SASS
/* foo
   bar
     baz
   bang */

foo bar {
  a: b;
}
SCSS

    assert_scss_to_scss <<SCSS
/* foo
   bar
     baz
   bang */

foo bar {
  a: b;
}
SCSS

    assert_renders <<SASS, <<SCSS
/* foo
 * bar
 *   baz
 * bang

foo bar
  a: b
SASS
/* foo
 * bar
 *   baz
 * bang */

foo bar {
  a: b;
}
SCSS
  end

  def test_nested_loud_comments
    assert_renders <<SASS, <<SCSS
foo
  bar: baz
  /* bip bop
   * beep boop
  bang: bizz
  /* bubble bubble
   * toil trouble
SASS
foo {
  bar: baz;
  /* bip bop
   * beep boop */
  bang: bizz;
  /* bubble bubble
   * toil trouble */
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
foo {
  bar: baz;
  /* bip bop
   * beep boop
   *   bap blimp */
  bang: bizz;
  /* bubble bubble
   * toil trouble
   *    gorp */
}
SCSS
foo
  bar: baz
  /* bip bop
     beep boop
       bap blimp
  bang: bizz
  /* bubble bubble
    toil trouble
       gorp
SASS
  end

  def test_preserves_double_star_comments
    assert_renders <<SASS, <<SCSS
/** foo
 *  bar
foo
  /** bip bop
   *  beep boop
SASS
/** foo
 *  bar */
foo {
  /** bip bop
   *  beep boop */
}
SCSS
  end

  def test_loud_comments_with_weird_indentation
    assert_scss_to_sass <<SASS, <<SCSS
foo
  /*      foo
   * bar
   *     baz
  a: b
SASS
foo {
  /* foo
bar
    baz */
  a: b;
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
foo {
  /*      foo
   * bar
   *     baz */
  a: b;
}
SCSS
foo
  /*      foo
     bar
         baz
  a: b
SASS
  end

  def test_loud_comment_containing_silent_comment
    assert_scss_to_sass <<SASS, <<SCSS
/*
 *// foo bar
SASS
/*
// foo bar
*/
SCSS
  end

  def test_silent_comment_containing_loud_comment
    assert_scss_to_sass <<SASS, <<SCSS
// /*
//  * foo bar
//  */
SASS
// /*
//  * foo bar
//  */
SCSS
  end

  def test_immediately_preceding_comments
    assert_renders <<SASS, <<SCSS
/* Foo
 * Bar
 * Baz
.foo#bar
  a: b
SASS
/* Foo
 * Bar
 * Baz */
.foo#bar {
  a: b;
}
SCSS

    assert_renders <<SASS, <<SCSS
// Foo
// Bar
// Baz
=foo
  a: b
SASS
// Foo
// Bar
// Baz
@mixin foo {
  a: b;
}
SCSS
  end

  def test_immediately_following_comments
    assert_sass_to_scss <<SCSS, <<SASS
.foobar {
  // trailing comment
  a: 1px;
}
SCSS
.foobar // trailing comment
  a: 1px
SASS

    assert_sass_to_scss <<SCSS, <<SASS
.foobar {
  // trailing comment
  a: 1px;
}
SCSS
.foobar  /* trailing comment */
  a: 1px
SASS
  end

  def test_debug
    assert_renders <<SASS, <<SCSS
foo
  @debug 12px
  bar: baz
SASS
foo {
  @debug 12px;
  bar: baz;
}
SCSS
  end

  def test_error
    assert_renders <<SASS, <<SCSS
foo
  @error "oh no!"
  bar: baz
SASS
foo {
  @error "oh no!";
  bar: baz;
}
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
  bar: baz;
}
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
    c: d;
  }

  bar: baz;
}
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
      a: b;
    }
    .bang {
      c: d;
      e: f;
    }
  }

  bar: baz;
}
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
      a: b;
    }
    .bang {
      c: d;
      e: f;
    }
    i: j;
  }

  bar: baz;
}
SCSS
  end

  def test_charset
    assert_renders <<SASS, <<SCSS
@charset "utf-8"
SASS
@charset "utf-8";
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
    a: b;
  }
  @for $c from 1 to 16 {
    d: e;
    f: g;
  }
}
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
    a: b;
  }
  @while 1 {
    d: e;
    f: g;
  }
}
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
    a: b;
  }
  @if $baz {
    d: e;
  }
  @else if $bang {
    f: g;
  }
  @else {
    h: i;
  }
}
SCSS
  end

  def test_each
    assert_renders <<SASS, <<SCSS
a
  @each $number in 1px 2px 3px 4px
    b: $number

c
  @each $str in foo, bar, baz, bang
    d: $str

c
  @each $key, $value in (foo: 1, bar: 2, baz: 3)
    \#{$key}: $value
SASS
a {
  @each $number in 1px 2px 3px 4px {
    b: $number;
  }
}

c {
  @each $str in foo, bar, baz, bang {
    d: $str;
  }
}

c {
  @each $key, $value in (foo: 1, bar: 2, baz: 3) {
    \#{$key}: $value;
  }
}
SCSS
  end

  def test_import
    assert_renders <<SASS, <<SCSS
@import foo

@import url(bar.css)

foo
  bar: baz
SASS
@import "foo";

@import url(bar.css);

foo {
  bar: baz;
}
SCSS

    assert_renders <<SASS, <<SCSS
@import foo.css

@import url(bar.css)

foo
  bar: baz
SASS
@import "foo.css";

@import url(bar.css);

foo {
  bar: baz;
}
SCSS
  end

  def test_import_as_directive_in_sass
    assert_equal "@import foo.css\n", to_sass('@import "foo.css"')
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

  def test_import_with_interpolation
    assert_renders <<SASS, <<SCSS
$family: unquote("Droid+Sans")

@import url("http://fonts.googleapis.com/css?family=\#{$family}")
SASS
$family: unquote("Droid+Sans");

@import url("http://fonts.googleapis.com/css?family=\#{$family}");
SCSS
  end

  def test_extend
    assert_renders <<SASS, <<SCSS
.foo
  @extend .bar
  @extend .baz:bang
SASS
.foo {
  @extend .bar;
  @extend .baz:bang;
}
SCSS
  end

  def test_comma_extendee
    assert_renders <<SASS, <<SCSS
.baz
  @extend .foo, .bar
SASS
.baz {
  @extend .foo, .bar;
}
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
    a: b;
  }
}
SCSS

    assert_scss_to_sass <<SASS, <<SCSS
=foo-bar
  baz
    a: b
SASS
@mixin foo-bar() {
  baz {
    a: b;
  }
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
@mixin foo-bar {
  baz {
    a: b;
  }
}
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
    a: $baz $bang;
  }
}
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
    a: $baz $bang;
  }
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS
@mixin foo-bar($baz, $bang: foo) {
  baz {
    a: $baz $bang;
  }
}
SCSS
=foo-bar($baz, $bang: foo)
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
  a: blip;
}
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
  a: blip;
}
SCSS
  end

  def test_mixin_include_with_keyword_args
    assert_renders <<SASS, <<SCSS
foo
  +foo-bar(12px, "blaz", $blip: blap, $bloop: blop)
  +foo-bar($blip: blap, $bloop: blop)
  a: blip
SASS
foo {
  @include foo-bar(12px, "blaz", $blip: blap, $bloop: blop);
  @include foo-bar($blip: blap, $bloop: blop);
  a: blip;
}
SCSS
  end

  def test_mixin_include_with_hyphen_conversion_keyword_arg
    assert_renders <<SASS, <<SCSS
foo
  +foo-bar($a-b_c: val)
  a: blip
SASS
foo {
  @include foo-bar($a-b_c: val);
  a: blip;
}
SCSS
  end

  def test_argless_function_definition
    assert_renders <<SASS, <<SCSS
@function foo()
  $var: 1 + 1
  @return $var
SASS
@function foo() {
  $var: 1 + 1;
  @return $var;
}
SCSS
  end

  def test_function_definition_without_defaults
    assert_renders <<SASS, <<SCSS
@function foo($var1, $var2)
  @if $var1
    @return $var1 + $var2
SASS
@function foo($var1, $var2) {
  @if $var1 {
    @return $var1 + $var2;
  }
}
SCSS
  end

  def test_function_definition_with_defaults
    assert_renders <<SASS, <<SCSS
@function foo($var1, $var2: foo)
  @if $var1
    @return $var1 + $var2
SASS
@function foo($var1, $var2: foo) {
  @if $var1 {
    @return $var1 + $var2;
  }
}
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
  val: $var1 $var2;
}
SCSS
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
  val: $var1 $var2;
}
SCSS
  end

  def test_multiple_variable_definitions
    assert_renders <<SASS, <<SCSS
$var1: foo
$var2: bar
$var3: baz

$var4: bip
$var5: bap
SASS
$var1: foo;
$var2: bar;
$var3: baz;

$var4: bip;
$var5: bap;
SCSS
  end

  def test_division_asserted_with_parens
    assert_renders <<SASS, <<SCSS
foo
  a: (1px / 2px)
SASS
foo {
  a: (1px / 2px);
}
SCSS
  end

  def test_division_not_asserted_when_unnecessary
    assert_renders <<SASS, <<SCSS
$var: 1px / 2px

foo
  a: $var
SASS
$var: 1px / 2px;

foo {
  a: $var;
}
SCSS

    assert_renders <<SASS, <<SCSS
$var: 1px

foo
  a: $var / 2px
SASS
$var: 1px;

foo {
  a: $var / 2px;
}
SCSS

    assert_renders <<SASS, <<SCSS
foo
  a: 1 + 1px / 2px
SASS
foo {
  a: 1 + 1px / 2px;
}
SCSS
  end

  def test_literal_slash
    assert_renders <<SASS, <<SCSS
foo
  a: 1px / 2px
SASS
foo {
  a: 1px / 2px;
}
SCSS
  end

  def test_directive_with_interpolation
    assert_renders <<SASS, <<SCSS
$baz: 12

@foo bar\#{$baz} qux
  a: b
SASS
$baz: 12;

@foo bar\#{$baz} qux {
  a: b;
}
SCSS
  end

  def test_media_with_interpolation
    assert_renders <<SASS, <<SCSS
$baz: 12

@media bar\#{$baz}
  a: b
SASS
$baz: 12;

@media bar\#{$baz} {
  a: b;
}
SCSS
  end

  def test_media_with_expressions
    assert_renders <<SASS, <<SCSS
$media1: screen
$media2: print
$var: -webkit-min-device-pixel-ratio
$val: 20

@media \#{$media1} and ($var + "-foo": $val + 5), only \#{$media2}
  a: b
SASS
$media1: screen;
$media2: print;
$var: -webkit-min-device-pixel-ratio;
$val: 20;

@media \#{$media1} and ($var + "-foo": $val + 5), only \#{$media2} {
  a: b;
}
SCSS
  end

  def test_media_with_feature
    assert_renders <<SASS, <<SCSS
@media screen and (-webkit-transform-3d)
  a: b
SASS
@media screen and (-webkit-transform-3d) {
  a: b;
}
SCSS
  end

  def test_supports_with_expressions
    assert_renders <<SASS, <<SCSS
$query: "(feature1: val)"
$feature: feature2
$val: val

@supports \#{$query} and ($feature: $val) or (not ($feature + 3: $val + 4))
  foo
    a: b
SASS
$query: "(feature1: val)";
$feature: feature2;
$val: val;

@supports \#{$query} and ($feature: $val) or (not ($feature + 3: $val + 4)) {
  foo {
    a: b;
  }
}
SCSS
  end

  # Hacks

  def test_declaration_hacks
    assert_renders <<SASS, <<SCSS
foo
  _name: val
  *name: val
  #name: val
  .name: val
  name/**/: val
  name/*\\**/: val
  name: val
SASS
foo {
  _name: val;
  *name: val;
  #name: val;
  .name: val;
  name/**/: val;
  name/*\\**/: val;
  name: val;
}
SCSS
  end

  def test_old_declaration_hacks
    assert_renders <<SASS, <<SCSS, :old => true
foo
  :_name val
  :*name val
  :#name val
  :.name val
  :name val
SASS
foo {
  _name: val;
  *name: val;
  #name: val;
  .name: val;
  name: val;
}
SCSS
  end

  def test_selector_hacks
    assert_selector_renders = lambda do |s|
      assert_renders <<SASS, <<SCSS
#{s}
  a: b
SASS
#{s} {
  a: b;
}
SCSS
    end

    assert_selector_renders['> E']
    assert_selector_renders['+ E']
    assert_selector_renders['~ E']
    assert_selector_renders['> > E']

    assert_selector_renders['E*']
    assert_selector_renders['E*.foo']
    assert_selector_renders['E*:hover']
  end

  def test_disallowed_colon_hack
    assert_raise_message(Sass::SyntaxError, 'The ":name: val" hack is not allowed in the Sass indented syntax') do
      to_sass("foo {:name: val;}", :syntax => :scss)
    end
  end

  def test_nested_properties
    assert_renders <<SASS, <<SCSS
div
  before: before
  background:
    color: blue
    repeat: no-repeat
  after: after
SASS
div {
  before: before;
  background: {
    color: blue;
    repeat: no-repeat;
  };
  after: after;
}

SCSS
  end

  def test_dasherize
    assert_sass_to_scss(<<SCSS, <<SASS, :dasherize => true)
@mixin under-scored-mixin($under-scored-arg: $under-scored-default) {
  bar: $under-scored-arg;
}

div {
  foo: under-scored-fn($under-scored-var + "before\#{$another-under-scored-var}after");
  @include under-scored-mixin($passed-arg);
  selector-\#{$under-scored-interp}: bold;
}

@if $under-scored {
  @for $for-var from $from-var to $to-var {
    @while $while-var == true {
      $while-var: false;
    }
  }
}
SCSS
=under_scored_mixin($under_scored_arg: $under_scored_default)
  bar: $under_scored_arg
div
  foo: under_scored_fn($under_scored_var + "before\#{$another_under_scored_var}after")
  +under_scored_mixin($passed_arg)
  selector-\#{$under_scored_interp}: bold
@if $under_scored
  @for $for_var from $from_var to $to_var
    @while $while_var == true
      $while_var : false
SASS
  end

  def test_loud_comment_conversion
    assert_renders(<<SASS, <<SCSS)
/*! \#{"interpolated"}
SASS
/*! \#{"interpolated"} */
SCSS
  end

  def test_content_conversion
    assert_renders(<<SASS, <<SCSS)
$color: blue

=context($class, $color: red)
  .\#{$class}
    background-color: $color
    @content
    border-color: $color

+context(parent)
  +context(child, $color: yellow)
    color: $color
SASS
$color: blue;

@mixin context($class, $color: red) {
  .\#{$class} {
    background-color: $color;
    @content;
    border-color: $color;
  }
}

@include context(parent) {
  @include context(child, $color: yellow) {
    color: $color;
  }
}
SCSS

  end

  def test_empty_content
    assert_scss_to_scss(<<SCSS)
@mixin foo {
  @content;
}

@include foo {}
SCSS
  end

  def test_placeholder_conversion
    assert_renders(<<SASS, <<SCSS)
#content a%foo.bar
  color: blue
SASS
#content a%foo.bar {
  color: blue;
}
SCSS
  end

  def test_reference_selector
    assert_renders(<<SASS, <<SCSS)
foo /bar|baz/ bang
  a: b
SASS
foo /bar|baz/ bang {
  a: b;
}
SCSS
  end

  def test_subject
    assert_renders(<<SASS, <<SCSS)
foo bar! baz
  a: b
SASS
foo bar! baz {
  a: b;
}
SCSS
  end

  def test_placeholder_interoplation_conversion
    assert_renders(<<SASS, <<SCSS)
$foo: foo

%\#{$foo}
  color: blue

.bar
  @extend %foo
SASS
$foo: foo;

%\#{$foo} {
  color: blue;
}

.bar {
  @extend %foo;
}
SCSS
  end

  def test_indent
    assert_renders <<SASS, <<SCSS, :indent => "    "
foo bar
    baz bang
        baz: bang
        bip: bop
    blat: boo
SASS
foo bar {
    baz bang {
        baz: bang;
        bip: bop;
    }
    blat: boo;
}
SCSS

    assert_renders <<SASS, <<SCSS, :indent => "\t"
foo bar
	baz bang
		baz: bang
		bip: bop
	blat: boo
SASS
foo bar {
	baz bang {
		baz: bang;
		bip: bop;
	}
	blat: boo;
}
SCSS

    assert_sass_to_scss <<SCSS, <<SASS, :indent => "    "
foo bar {
    baz bang {
        baz: bang;
        bip: bop;
    }
    blat: boo;
}
SCSS
foo bar
  baz bang
    baz: bang
    bip: bop
  blat: boo
SASS

    assert_sass_to_scss <<SCSS, <<SASS, :indent => "\t"
foo bar {
	baz bang {
		baz: bang;
		bip: bop;
	}
	blat: boo;
}
SCSS
foo bar
  baz bang
    baz: bang
    bip: bop
  blat: boo
SASS

    assert_scss_to_sass <<SASS, <<SCSS, :indent => "    "
foo bar
    baz bang
        baz: bang
        bip: bop
    blat: boo
SASS
foo bar {
  baz bang {
    baz: bang;
    bip: bop;
  }
  blat: boo;
}
SCSS

    assert_scss_to_sass <<SASS, <<SCSS, :indent => "\t"
foo bar
	baz bang
		baz: bang
		bip: bop
	blat: boo
SASS
foo bar {
  baz bang {
    baz: bang;
    bip: bop;
  }
  blat: boo;
}
SCSS
  end

  def test_extend_with_optional
    assert_renders <<SASS, <<SCSS
foo
  @extend .bar !optional
SASS
foo {
  @extend .bar !optional;
}
SCSS
  end

  def test_mixin_var_args
    assert_renders <<SASS, <<SCSS
=foo($args...)
  a: b

=bar($a, $args...)
  a: b

.foo
  +foo($list...)
  +bar(1, $list...)
SASS
@mixin foo($args...) {
  a: b;
}

@mixin bar($a, $args...) {
  a: b;
}

.foo {
  @include foo($list...);
  @include bar(1, $list...);
}
SCSS
  end

  def test_mixin_var_kwargs
    assert_renders <<SASS, <<SCSS
=foo($a: b, $c: d)
  a: $a
  c: $c

.foo
  +foo($list..., $map...)
  +foo(pos, $list..., $kwd: val, $map...)
SASS
@mixin foo($a: b, $c: d) {
  a: $a;
  c: $c;
}

.foo {
  @include foo($list..., $map...);
  @include foo(pos, $list..., $kwd: val, $map...);
}
SCSS
  end

  def test_function_var_args
    assert_renders <<SASS, <<SCSS
@function foo($args...)
  @return foo

@function bar($a, $args...)
  @return bar

.foo
  a: foo($list...)
  b: bar(1, $list...)
SASS
@function foo($args...) {
  @return foo;
}

@function bar($a, $args...) {
  @return bar;
}

.foo {
  a: foo($list...);
  b: bar(1, $list...);
}
SCSS
  end

  def test_function_var_kwargs
    assert_renders <<SASS, <<SCSS
@function foo($a: b, $c: d)
  @return foo

.foo
  a: foo($list..., $map...)
  b: foo(pos, $list..., $kwd: val, $map...)
SASS
@function foo($a: b, $c: d) {
  @return foo;
}

.foo {
  a: foo($list..., $map...);
  b: foo(pos, $list..., $kwd: val, $map...);
}
SCSS
  end

  def test_at_root
    assert_renders <<SASS, <<SCSS
.foo
  @at-root
    .bar
      a: b
    .baz
      c: d
SASS
.foo {
  @at-root {
    .bar {
      a: b;
    }
    .baz {
      c: d;
    }
  }
}
SCSS
  end

  def test_at_root_with_selector
    assert_renders <<SASS, <<SCSS
.foo
  @at-root .bar
    a: b
SASS
.foo {
  @at-root .bar {
    a: b;
  }
}
SCSS
  end

  def test_at_root_without
    assert_renders <<SASS, <<SCSS
.foo
  @at-root (without: media rule)
    a: b
SASS
.foo {
  @at-root (without: media rule) {
    a: b;
  }
}
SCSS
  end

  def test_at_root_with
    assert_renders <<SASS, <<SCSS
.foo
  @at-root (with: media rule)
    a: b
SASS
.foo {
  @at-root (with: media rule) {
    a: b;
  }
}
SCSS
  end

  def test_function_var_kwargs_with_list
    assert_renders <<SASS, <<SCSS
@function foo($a: b, $c: d)
  @return $a, $c

.foo
  a: foo($list..., $map...)
SASS
@function foo($a: b, $c: d) {
  @return $a, $c;
}

.foo {
  a: foo($list..., $map...);
}
SCSS
  end

  def test_keyframes
    assert_renders(<<SASS, <<SCSS)
@keyframes identifier
  0%
    top: 0
    left: 0
  30%
    top: 50px
  68%, 72%
    left: 50px
  100%
    top: 100px
    left: 100%
SASS
@keyframes identifier {
  0% {
    top: 0;
    left: 0;
  }
  30% {
    top: 50px;
  }
  68%, 72% {
    left: 50px;
  }
  100% {
    top: 100px;
    left: 100%;
  }
}
SCSS
  end

  ## Regression Tests

  def test_list_in_args
    assert_renders(<<SASS, <<SCSS)
+mixin((a, b, c))

+mixin($arg: (a, b, c))

+mixin(a, b, (c, d, e)...)
SASS
@include mixin((a, b, c));

@include mixin($arg: (a, b, c));

@include mixin(a, b, (c, d, e)...);
SCSS
  end

  def test_media_query_with_expr
    assert_renders <<SASS, <<SCSS
@media foo and (bar: baz)
  a: b
SASS
@media foo and (bar: baz) {
  a: b;
}
SCSS
  end

  def test_nested_if_statements
    assert_renders(<<SASS, <<SCSS)
@if $foo
  one
    a: b
@else
  @if $bar
    two
      a: b
  @else
    three
      a: b
SASS
@if $foo {
  one {
    a: b;
  }
}
@else {
  @if $bar {
    two {
      a: b;
    }
  }
  @else {
    three {
      a: b;
    }
  }
}
SCSS
  end

  def test_comment_indentation
    assert_renders(<<SASS, <<SCSS, :indent => '    ')
foo
    // bar
    /* baz
    a: b
SASS
foo {
    // bar
    /* baz */
    a: b;
}
SCSS
  end

  def test_keyword_arguments
    assert_renders(<<SASS, <<SCSS, :dasherize => true)
$foo: foo($dash-ed: 2px)
SASS
$foo: foo($dash-ed: 2px);
SCSS
    assert_scss_to_sass(<<SASS, <<SCSS, :dasherize => true)
$foo: foo($dash-ed: 2px)
SASS
$foo: foo($dash_ed: 2px);
SCSS
    assert_sass_to_scss(<<SCSS, <<SASS, :dasherize => true)
$foo: foo($dash-ed: 2px);
SCSS
$foo: foo($dash_ed: 2px)
SASS
    assert_renders(<<SASS, <<SCSS)
$foo: foo($under_scored: 1px)
SASS
$foo: foo($under_scored: 1px);
SCSS
    assert_renders(<<SASS, <<SCSS)
$foo: foo($dash-ed: 2px, $under_scored: 1px)
SASS
$foo: foo($dash-ed: 2px, $under_scored: 1px);
SCSS
  end

  def test_ambiguous_negation
    assert_renders(<<SASS, <<SCSS, :indent => '    ')
foo
    ok: -$foo
    comma: 10px, -$foo
    needs-parens: 10px (-$foo)
SASS
foo {
    ok: -$foo;
    comma: 10px, -$foo;
    needs-parens: 10px (-$foo);
}
SCSS
  end

  def test_variable_with_global
    assert_renders(<<SASS, <<SCSS)
$var: 1

foo
  $var: 2 !global
  $var: 3 !global !default
SASS
$var: 1;

foo {
  $var: 2 !global;
  $var: 3 !global !default;
}
SCSS
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
      "Expected SCSS to transform to #{scss == in_scss ? 'itself' : 'SCSS'}")
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
    Sass::Util.silence_sass_warnings do
      Sass::Engine.new(scss, options).to_tree.to_sass(options)
    end
  end

  def to_scss(sass, options = {})
    Sass::Util.silence_sass_warnings do
      Sass::Engine.new(sass, options).to_tree.to_scss(options)
    end
  end
end
