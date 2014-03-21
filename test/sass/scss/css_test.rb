#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/test_helper'
require 'sass/scss/css_parser'

# These tests just test the parsing of CSS
# (both standard and any hacks we intend to support).
# Tests of SCSS-specific behavior go in scss_test.rb.
class ScssCssTest < Test::Unit::TestCase
  include ScssTestHelper

  def test_basic_scss
    assert_parses <<SCSS
selector {
  property: value;
  property2: value; }
SCSS

    assert_equal <<CSS, render('sel{p:v}')
sel {
  p: v; }
CSS
  end

  def test_empty_rule
    assert_equal "", render("#foo .bar {}")
    assert_equal "", render(<<SCSS)
#foo .bar {
}
SCSS
  end

  def test_cdo_and_cdc_ignored_at_toplevel
    assert_equal <<CSS, render(<<SCSS)
foo {
  bar: baz; }

bar {
  bar: baz; }

baz {
  bar: baz; }
CSS
foo {bar: baz}
<!--
bar {bar: baz}
-->
baz {bar: baz}
SCSS
  end

  if Sass::Util.ruby1_8?
    def test_unicode
      assert_parses <<SCSS
@charset "UTF-8";
foo {
  bar: föö bâr; }
SCSS
      assert_parses <<SCSS
foo {
  bar: föö bâr; }
SCSS
    end
  else
    def test_unicode
      assert_parses <<SCSS
@charset "UTF-8";
foo {
  bar: föö bâr; }
SCSS
      assert_equal <<CSS, render(<<SCSS)
@charset "UTF-8";
foo {
  bar: föö bâr; }
CSS
foo {
  bar: föö bâr; }
SCSS
    end
  end

  def test_invisible_comments
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: d; }
CSS
foo {a: /* b; c: */ d}
SCSS
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: d; }
CSS
foo {a /*: b; c */: d}
SCSS
  end

  def test_crazy_comments
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/t040109-c17-comments-00-b.xht
    assert_equal <<CSS, render(<<SCSS)
/* This is a CSS comment. */
.one {
  color: green; }

/* Another comment */
/* The following should not be used:
.two {color: red;} */
.three {
  color: green;
  /* color: red; */ }

/**
.four {color: red;} */
.five {
  color: green; }

/**/
.six {
  color: green; }

/*********/
.seven {
  color: green; }

/* a comment **/
.eight {
  color: green; }
CSS
/* This is a CSS comment. */
.one {color: green;} /* Another comment */
/* The following should not be used:
.two {color: red;} */
.three {color: green; /* color: red; */}
/**
.four {color: red;} */
.five {color: green;}
/**/
.six {color: green;}
/*********/
.seven {color: green;}
/* a comment **/
.eight {color: green;}
SCSS
  end

  def test_rule_comments
    assert_parses <<SCSS
/* Foo */
.foo {
  a: b; }
SCSS
    assert_equal <<CSS, render(<<SCSS)
/* Foo
 * Bar */
.foo {
  a: b; }
CSS
/* Foo
 * Bar */.foo {
  a: b; }
SCSS
  end

  def test_property_comments
    assert_parses <<SCSS
.foo {
  /* Foo */
  a: b; }
SCSS
    assert_equal <<CSS, render(<<SCSS)
.foo {
  /* Foo
   * Bar */
  a: b; }
CSS
.foo {
  /* Foo
   * Bar */a: b; }
SCSS
  end

  def test_selector_comments
    assert_equal <<CSS, render(<<SCSS)
.foo #bar:baz(bip) {
  a: b; }
CSS
.foo /* .a #foo */ #bar:baz(/* bang )*/ bip) {
  a: b; }
SCSS
  end

  def test_lonely_comments
    assert_parses <<SCSS
/* Foo
 * Bar */
SCSS
    assert_parses <<SCSS
.foo {
  /* Foo
   * Bar */ }
SCSS
  end

  def test_multiple_comments
    assert_parses <<SCSS
/* Foo
 * Bar */
/* Baz
 * Bang */
SCSS
    assert_parses <<SCSS
.foo {
  /* Foo
   * Bar */
  /* Baz
   * Bang */ }
SCSS
    assert_equal <<CSS, render(<<SCSS)
.foo {
  /* Foo Bar */
  /* Baz Bang */ }
CSS
.foo {
  /* Foo Bar *//* Baz Bang */ }
SCSS
  end

  def test_bizarrely_formatted_comments
    assert_parses <<SCSS
.foo {
  /* Foo
Bar
  Baz */
  a: b; }
SCSS
    assert_parses <<SCSS
.foo {
    /* Foo
Bar
  Baz */
  a: b; }
SCSS
    assert_equal <<CSS, render(<<SCSS)
.foo {
   /* Foo
Bar */
  a: b; }
CSS
.foo {/* Foo
   Bar */
  a: b; }
SCSS
    assert_equal <<CSS, render(<<SCSS)
.foo {
    /* Foo
 Bar
Baz */
  a: b; }
CSS
.foo {/* Foo
   Bar
  Baz */
  a: b; }
SCSS
  end

  ## Declarations

  def test_vendor_properties
    assert_parses <<SCSS
foo {
  -moz-foo-bar: blat;
  -o-flat-blang: wibble; }
SCSS
  end

  def test_empty_declarations
    assert_equal <<CSS, render(<<SCSS)
foo {
  bar: baz; }
CSS
foo {;;;;
  bar: baz;;;;
  ;;}
SCSS
  end

  def test_basic_property_types
    assert_parses <<SCSS
foo {
  a: 2;
  b: 2.3em;
  c: 50%;
  d: "fraz bran";
  e: flanny-blanny-blan;
  f: url(http://sass-lang.com);
  g: U+ffa?;
  h: #aabbcc; }
SCSS
  end

  def test_functions
    assert_parses <<SCSS
foo {
  a: foo-bar(12);
  b: -foo-bar-baz(13, 14 15); }
SCSS
  end

  def test_unary_minus
    assert_parses <<SCSS
foo {
  a: -2;
  b: -2.3em;
  c: -50%;
  d: -foo(bar baz); }
SCSS
  end

  def test_operators
    assert_parses <<SCSS
foo {
  a: foo bar baz;
  b: foo, #aabbcc, -12;
  c: 1px/2px/-3px;
  d: foo bar, baz/bang; }
SCSS
  end

  def test_important
    assert_parses <<SCSS
foo {
  a: foo !important;
  b: foo bar !important;
  b: foo, bar !important; }
SCSS
  end

  def test_initial_hyphen
    assert_parses <<SCSS
foo {
  a: -moz-bar-baz;
  b: foo -o-bar-baz; }
SCSS
  end

  def test_ms_long_filter_syntax
    assert_equal <<CSS, render(<<SCSS)
foo {
  filter: progid:DXImageTransform.Microsoft.gradient(GradientType=1, startColorstr=#c0ff3300, endColorstr=#ff000000);
  filter: progid:DXImageTransform.Microsoft.gradient(GradientType=1, startColorstr=#c0ff3300, endColorstr=#ff000000); }
CSS
foo {
  filter: progid:DXImageTransform.Microsoft.gradient(GradientType=1, startColorstr=#c0ff3300, endColorstr=#ff000000);
  filter:progid:DXImageTransform.Microsoft.gradient(GradientType=1, startColorstr=#c0ff3300, endColorstr=#ff000000); }
SCSS
  end

  def test_ms_short_filter_syntax
    assert_parses <<SCSS
foo {
  filter: alpha(opacity=20);
  filter: alpha(opacity=20, enabled=true);
  filter: blaznicate(foo=bar, baz=bang bip, bart=#fa4600); }
SCSS
  end

  def test_declaration_hacks
    assert_parses <<SCSS
foo {
  _name: val;
  *name: val;
  :name: val;
  .name: val;
  #name: val;
  name/**/: val;
  name/*\\**/: val;
  name: val; }
SCSS
  end

  def test_trailing_hash_hack
    assert_parses <<SCSS
foo {
  foo: bar;
  #baz: bang;
  #bip: bop; }
SCSS
  end

  def test_zero_arg_functions
    assert_parses <<SCSS
foo {
  a: foo();
  b: bar baz-bang() bip; }
SCSS
  end

  def test_expression_function
    assert_parses <<SCSS
foo {
  a: 12px expression(1 + (3 / Foo.bar("baz" + "bang") + function() {return 12;}) % 12); }
SCSS
  end

  def test_calc_function
    assert_parses <<SCSS
foo {
  a: 12px calc(100%/3 - 2*1em - 2*1px);
  b: 12px -moz-calc(100%/3 - 2*1em - 2*1px);
  b: 12px -webkit-calc(100%/3 - 2*1em - 2*1px);
  b: 12px -foobar-calc(100%/3 - 2*1em - 2*1px); }
SCSS
  end

  def test_element_function
    assert_parses <<SCSS
foo {
  a: -moz-element(#foo);
  b: -webkit-element(#foo);
  b: -foobar-element(#foo); }
SCSS
  end

  def test_unary_ops
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: -0.5em;
  b: +0.5em;
  c: -foo(12px);
  d: +foo(12px); }
CSS
foo {
  a: -0.5em;
  b: +0.5em;
  c: -foo(12px);
  d: +foo(12px); }
SCSS
  end

  def test_css_string_escapes
    assert_parses <<SCSS
foo {
  a: "\\foo bar";
  b: "foo\\ bar";
  c: "\\2022 \\0020";
  d: "foo\\\\bar";
  e: "foo\\"'bar"; }
SCSS
  end

  def test_css_ident_escapes
    assert_parses <<SCSS
foo {
  a: \\foo bar;
  b: foo\\ bar;
  c: \\2022 \\0020;
  d: foo\\\\bar;
  e: foo\\"\\'bar; }
SCSS
  end

  ## Directives

  def test_namespace_directive
    assert_parses '@namespace "http://www.w3.org/Profiles/xhtml1-strict";'
    assert_parses '@namespace url(http://www.w3.org/Profiles/xhtml1-strict);'
    assert_parses '@namespace html url("http://www.w3.org/Profiles/xhtml1-strict");'
  end

  def test_media_directive
    assert_parses <<SCSS
@media all {
  rule1 {
    prop: val; }

  rule2 {
    prop: val; } }
SCSS
    assert_parses <<SCSS
@media screen, print {
  rule1 {
    prop: val; }

  rule2 {
    prop: val; } }
SCSS
  end

  def test_media_directive_with_keywords
    assert_parses <<SCSS
@media screen and (-webkit-min-device-pixel-ratio: 0) {
  a: b; }
SCSS
    assert_parses <<SCSS
@media only screen, print and (foo: 0px) and (bar: flam(12px solid)) {
  a: b; }
SCSS
  end

  def test_import_directive
    assert_parses '@import "foo.css";'
    assert_parses "@import 'foo.css';"
    assert_parses '@import url("foo.css");'
    assert_parses "@import url('foo.css');"
    assert_parses '@import url(foo.css);'
  end

  def test_string_import_directive_with_media
    assert_parses '@import "foo.css" screen;'
    assert_parses '@import "foo.css" screen, print;'
    assert_parses '@import "foo.css" screen, print and (foo: 0);'
    assert_parses '@import "foo.css" screen, only print, screen and (foo: 0);'
  end

  def test_url_import_directive_with_media
    assert_parses '@import url("foo.css") screen;'
    assert_parses '@import url("foo.css") screen, print;'
    assert_parses '@import url("foo.css") screen, print and (foo: 0);'
    assert_parses '@import url("foo.css") screen, only print, screen and (foo: 0);'
  end

  def test_page_directive
    assert_parses <<SCSS
@page {
  prop1: val;
  prop2: val; }
SCSS
    assert_parses <<SCSS
@page flap {
  prop1: val;
  prop2: val; }
SCSS
    assert_parses <<SCSS
@page :first {
  prop1: val;
  prop2: val; }
SCSS
    assert_parses <<SCSS
@page flap:first {
  prop1: val;
  prop2: val; }
SCSS
  end

  def test_blockless_directive_without_semicolon
    assert_equal "@foo \"bar\";\n", render('@foo "bar"')
  end

  def test_directive_with_lots_of_whitespace
    assert_equal "@foo \"bar\";\n", render('@foo    "bar"  ;')
  end

  def test_empty_blockless_directive
    assert_parses "@foo;"
  end

  def test_multiple_blockless_directives
    assert_parses <<SCSS
@foo bar;
@bar baz;
SCSS
  end

  def test_empty_block_directive
    assert_parses "@foo {}"
    assert_equal "@foo {}\n", render(<<SCSS)
@foo {
}
SCSS
  end

  def test_multiple_block_directives
    assert_parses <<SCSS
@foo bar {
  a: b; }
@bar baz {
  c: d; }
SCSS
  end

  def test_block_directive_with_rule_and_property
    assert_parses <<SCSS
@foo {
  rule {
    a: b; }

  a: b; }
SCSS
  end

  def test_block_directive_with_semicolon
    assert_equal <<CSS, render(<<SCSS)
@foo {
  a: b; }
@bar {
  a: b; }
CSS
@foo {a:b};
@bar {a:b};
SCSS
  end

  def test_moz_document_directive
    assert_equal <<CSS, render(<<SCSS)
@-moz-document url(http://www.w3.org/),
               url-prefix(http://www.w3.org/Style/),
               domain(mozilla.org),
               regexp("^https:.*") {
  .foo {
    a: b; } }
CSS
@-moz-document url(http://www.w3.org/),
               url-prefix(http://www.w3.org/Style/),
               domain(mozilla.org),
               regexp("^https:.*") {
  .foo {a: b}
}
SCSS
  end

  def test_supports
    assert_equal <<CSS, render(<<SCSS)
@supports (a: b) and (c: d) or (not (d: e)) and ((not (f: g)) or (not ((h: i) and (j: k)))) {
  .foo {
    a: b; } }
@supports (a: b) {
  .foo {
    a: b; } }
CSS
@supports (a: b) and (c: d) or (not (d: e)) and ((not (f: g)) or (not ((h: i) and (j: k)))) {
  .foo {
    a: b;
  }
}

@supports (a: b) {
  .foo {
    a: b;
  }
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
@-prefix-supports (a: b) and (c: d) or (not (d: e)) and ((not (f: g)) or (not ((h: i) and (j: k)))) {
  .foo {
    a: b; } }
CSS
@-prefix-supports (a: b) and (c: d) or (not (d: e)) and ((not (f: g)) or (not ((h: i) and (j: k)))) {
  .foo {
    a: b;
  }
}
SCSS
  end

  ## Selectors

  # Taken from http://dev.w3.org/csswg/selectors4/#overview
  def test_summarized_selectors_with_element
    assert_selector_parses('*')
    assert_selector_parses('E')
    assert_selector_parses('E:not(s)')
    assert_selector_parses('E:not(s1, s2)')
    assert_selector_parses('E:matches(s1, s2)')
    assert_selector_parses('E.warning')
    assert_selector_parses('E#myid')
    assert_selector_parses('E[foo]')
    assert_selector_parses('E[foo="bar"]')
    assert_selector_parses('E[foo="bar" i]')
    assert_selector_parses('E[foo~="bar"]')
    assert_selector_parses('E[foo^="bar"]')
    assert_selector_parses('E[foo$="bar"]')
    assert_selector_parses('E[foo*="bar"]')
    assert_selector_parses('E[foo|="en"]')
    assert_selector_parses('E:dir(ltr)')
    assert_selector_parses('E:lang(fr)')
    assert_selector_parses('E:lang(zh, *-hant)')
    assert_selector_parses('E:any-link')
    assert_selector_parses('E:link')
    assert_selector_parses('E:visited')
    assert_selector_parses('E:local-link')
    assert_selector_parses('E:local-link(0)')
    assert_selector_parses('E:target')
    assert_selector_parses('E:scope')
    assert_selector_parses('E:current')
    assert_selector_parses('E:current(s)')
    assert_selector_parses('E:past')
    assert_selector_parses('E:future')
    assert_selector_parses('E:active')
    assert_selector_parses('E:hover')
    assert_selector_parses('E:focus')
    assert_selector_parses('E:enabled')
    assert_selector_parses('E:disabled')
    assert_selector_parses('E:checked')
    assert_selector_parses('E:indeterminate')
    assert_selector_parses('E:default')
    assert_selector_parses('E:in-range')
    assert_selector_parses('E:out-of-range')
    assert_selector_parses('E:required')
    assert_selector_parses('E:optional')
    assert_selector_parses('E:read-only')
    assert_selector_parses('E:read-write')
    assert_selector_parses('E:root')
    assert_selector_parses('E:empty')
    assert_selector_parses('E:first-child')
    assert_selector_parses('E:nth-child(n)')
    assert_selector_parses('E:last-child')
    assert_selector_parses('E:nth-last-child(n)')
    assert_selector_parses('E:only-child')
    assert_selector_parses('E:first-of-type')
    assert_selector_parses('E:nth-of-type(n)')
    assert_selector_parses('E:last-of-type')
    assert_selector_parses('E:nth-last-of-type(n)')
    assert_selector_parses('E:only-of-type')
    assert_selector_parses('E:nth-match(n of selector)')
    assert_selector_parses('E:nth-last-match(n of selector)')
    assert_selector_parses('E:column(selector)')
    assert_selector_parses('E:nth-column(n)')
    assert_selector_parses('E:nth-last-column(n)')
    assert_selector_parses('E F')
    assert_selector_parses('E > F')
    assert_selector_parses('E + F')
    assert_selector_parses('E ~ F')
    assert_selector_parses('E /foo/ F')
    assert_selector_parses('E! > F')

    assert_selector_parses('E /ns|foo/ F')
    assert_selector_parses('E /*|foo/ F')
  end

  # Taken from http://dev.w3.org/csswg/selectors4/#overview, but without element
  # names.
  def test_more_summarized_selectors
    assert_selector_parses(':not(s)')
    assert_selector_parses(':not(s1, s2)')
    assert_selector_parses(':matches(s1, s2)')
    assert_selector_parses('.warning')
    assert_selector_parses('#myid')
    assert_selector_parses('[foo]')
    assert_selector_parses('[foo="bar"]')
    assert_selector_parses('[foo="bar" i]')
    assert_selector_parses('[foo~="bar"]')
    assert_selector_parses('[foo^="bar"]')
    assert_selector_parses('[foo$="bar"]')
    assert_selector_parses('[foo*="bar"]')
    assert_selector_parses('[foo|="en"]')
    assert_selector_parses(':dir(ltr)')
    assert_selector_parses(':lang(fr)')
    assert_selector_parses(':lang(zh, *-hant)')
    assert_selector_parses(':any-link')
    assert_selector_parses(':link')
    assert_selector_parses(':visited')
    assert_selector_parses(':local-link')
    assert_selector_parses(':local-link(0)')
    assert_selector_parses(':target')
    assert_selector_parses(':scope')
    assert_selector_parses(':current')
    assert_selector_parses(':current(s)')
    assert_selector_parses(':past')
    assert_selector_parses(':future')
    assert_selector_parses(':active')
    assert_selector_parses(':hover')
    assert_selector_parses(':focus')
    assert_selector_parses(':enabled')
    assert_selector_parses(':disabled')
    assert_selector_parses(':checked')
    assert_selector_parses(':indeterminate')
    assert_selector_parses(':default')
    assert_selector_parses(':in-range')
    assert_selector_parses(':out-of-range')
    assert_selector_parses(':required')
    assert_selector_parses(':optional')
    assert_selector_parses(':read-only')
    assert_selector_parses(':read-write')
    assert_selector_parses(':root')
    assert_selector_parses(':empty')
    assert_selector_parses(':first-child')
    assert_selector_parses(':nth-child(n)')
    assert_selector_parses(':last-child')
    assert_selector_parses(':nth-last-child(n)')
    assert_selector_parses(':only-child')
    assert_selector_parses(':first-of-type')
    assert_selector_parses(':nth-of-type(n)')
    assert_selector_parses(':last-of-type')
    assert_selector_parses(':nth-last-of-type(n)')
    assert_selector_parses(':only-of-type')
    assert_selector_parses(':nth-match(n of selector)')
    assert_selector_parses(':nth-last-match(n of selector)')
    assert_selector_parses(':column(selector)')
    assert_selector_parses(':nth-column(n)')
    assert_selector_parses(':nth-last-column(n)')
  end

  def test_attribute_selectors_with_identifiers
    assert_selector_parses('[foo~=bar]')
    assert_selector_parses('[foo^=bar]')
    assert_selector_parses('[foo$=bar]')
    assert_selector_parses('[foo*=bar]')
    assert_selector_parses('[foo|=en]')
  end

  def test_nth_selectors
    assert_selector_parses(':nth-child(-n)')
    assert_selector_parses(':nth-child(+n)')

    assert_selector_parses(':nth-child(even)')
    assert_selector_parses(':nth-child(odd)')

    assert_selector_parses(':nth-child(50)')
    assert_selector_parses(':nth-child(-50)')
    assert_selector_parses(':nth-child(+50)')

    assert_selector_parses(':nth-child(2n+3)')
    assert_selector_parses(':nth-child(2n-3)')
    assert_selector_parses(':nth-child(+2n-3)')
    assert_selector_parses(':nth-child(-2n+3)')
    assert_selector_parses(':nth-child(-2n+ 3)')

    assert_equal(<<CSS, render(<<SCSS))
:nth-child(2n + 3) {
  a: b; }
CSS
:nth-child( 2n + 3 ) {
  a: b; }
SCSS
  end

  def test_selectors_containing_selectors
    assert_selector_can_contain_selectors(':not(<sel>)')
    assert_selector_can_contain_selectors(':current(<sel>)')
    assert_selector_can_contain_selectors(':nth-match(n of <sel>)')
    assert_selector_can_contain_selectors(':nth-last-match(n of <sel>)')
    assert_selector_can_contain_selectors(':column(<sel>)')
    assert_selector_can_contain_selectors(':-moz-any(<sel>)')
  end

  def assert_selector_can_contain_selectors(sel)
    try = lambda {|subsel| assert_selector_parses(sel.gsub('<sel>', subsel))}

    try['foo|bar']
    try['*|bar']

    try['foo|*']
    try['*|*']

    try['#blah']
    try['.blah']

    try['[foo]']
    try['[foo^="bar"]']
    try['[baz|foo~="bar"]']

    try[':hover']
    try[':nth-child(2n + 3)']

    try['h1, h2, h3']
    try['#foo, bar, [baz]']

    # Not technically allowed for most selectors, but what the heck
    try[':not(#foo)']
    try['a#foo.bar']
    try['#foo .bar > baz']
  end

  def test_namespaced_selectors
    assert_selector_parses('foo|E')
    assert_selector_parses('*|E')
    assert_selector_parses('foo|*')
    assert_selector_parses('*|*')
  end

  def test_namespaced_attribute_selectors
    assert_selector_parses('[foo|bar=baz]')
    assert_selector_parses('[*|bar=baz]')
    assert_selector_parses('[foo|bar|=baz]')
  end

  def test_comma_selectors
    assert_selector_parses('E, F')
    assert_selector_parses('E F, G H')
    assert_selector_parses('E > F, G > H')
  end

  def test_selectors_with_newlines
    assert_selector_parses("E,\nF")
    assert_selector_parses("E\nF")
    assert_selector_parses("E, F\nG, H")
  end

  def test_expression_fallback_selectors
    assert_selector_parses('0%')
    assert_selector_parses('60%')
    assert_selector_parses('100%')
    assert_selector_parses('12px')
    assert_selector_parses('"foo"')
  end

  def test_functional_pseudo_selectors
    assert_selector_parses(':foo("bar")')
    assert_selector_parses(':foo(bar)')
    assert_selector_parses(':foo(12px)')
    assert_selector_parses(':foo(+)')
    assert_selector_parses(':foo(-)')
    assert_selector_parses(':foo(+"bar")')
    assert_selector_parses(':foo(-++--baz-"bar"12px)')
  end

  def test_selector_hacks
    assert_selector_parses('> E')
    assert_selector_parses('+ E')
    assert_selector_parses('~ E')
    assert_selector_parses('> > E')
    assert_equal <<CSS, render(<<SCSS)
> > E {
  a: b; }
CSS
>> E {
  a: b; }
SCSS

    assert_selector_parses('E*')
    assert_selector_parses('E*.foo')
    assert_selector_parses('E*:hover')
  end

  def test_spaceless_combo_selectors
    assert_equal "E > F {\n  a: b; }\n", render("E>F { a: b;} ")
    assert_equal "E ~ F {\n  a: b; }\n", render("E~F { a: b;} ")
    assert_equal "E + F {\n  a: b; }\n", render("E+F { a: b;} ")
  end

  ## Errors

  def test_invalid_directives
    assert_not_parses("identifier", '@<err> import "foo";')
    assert_not_parses("identifier", '@<err>12 "foo";')
  end

  def test_invalid_classes
    assert_not_parses("class name", 'p.<err> foo {a: b}')
    assert_not_parses("class name", 'p.<err>1foo {a: b}')
  end

  def test_invalid_ids
    assert_not_parses("id name", 'p#<err> foo {a: b}')
  end

  def test_no_properties_at_toplevel
    assert_not_parses('pseudoclass or pseudoelement', 'a:<err> b;')
  end

  def test_no_scss_directives
    assert_parses('@import "foo.sass";')
    assert_parses <<SCSS
@mixin foo {
  a: b; }
SCSS
  end

  def test_no_variables
    assert_not_parses("selector or at-rule", "<err>$var = 12;")
    assert_not_parses('"}"', "foo { <err>!var = 12; }")
  end

  def test_no_parent_selectors
    assert_not_parses('"{"', "foo <err>&.bar {a: b}")
  end

  def test_no_selector_interpolation
    assert_not_parses('"{"', 'foo <err>#{"bar"}.baz {a: b}')
  end

  def test_no_prop_name_interpolation
    assert_not_parses('":"', 'foo {a<err>#{"bar"}baz: b}')
  end

  def test_no_prop_val_interpolation
    assert_not_parses('"}"', 'foo {a: b <err>#{"bar"} c}')
  end

  def test_no_string_interpolation
    assert_parses <<SCSS
foo {
  a: "bang \#{1 +    " bar "} bip"; }
SCSS
  end

  def test_no_sass_script_values
    assert_not_parses('"}"', 'foo {a: b <err>* c}')
  end

  def test_no_nested_rules
    assert_not_parses('":"', 'foo {bar <err>{a: b}}')
    assert_not_parses('"}"', 'foo {<err>[bar=baz] {a: b}}')
  end

  def test_no_nested_properties
    assert_not_parses('expression (e.g. 1px, bold)', 'foo {bar: <err>{a: b}}')
    assert_not_parses('expression (e.g. 1px, bold)', 'foo {bar: bang <err>{a: b}}')
  end

  def test_no_nested_directives
    assert_not_parses('"}"', 'foo {<err>@bar {a: b}}')
  end

  def test_error_with_windows_newlines
    render <<SCSS
foo {bar}\r
baz {a: b}
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "foo {bar": expected ":", was "}"', e.message
    assert_equal 1, e.sass_line
  end

  ## Regressions

  def test_double_space_string
    assert_equal(<<CSS, render(<<SCSS))
.a {
  content: "  a"; }
CSS
.a {
  content: "  a";
}
SCSS
  end

  def test_very_long_number_with_important_doesnt_take_forever
    assert_equal(<<CSS, render(<<SCSS))
.foo {
  width: 97.916666666666666666666666666667% !important; }
CSS
.foo {
  width: 97.916666666666666666666666666667% !important;
}
SCSS
  end

  def test_selector_without_closing_bracket
    assert_not_parses('"]"', "foo[bar <err>{a: b}")
  end

  def test_closing_line_comment_end_with_compact_output
    assert_equal(<<CSS, render(<<SCSS, :style => :compact))
/* foo */
bar { baz: bang; }
CSS
/*
 * foo
 */
bar {baz: bang}
SCSS
  end

  def test_single_line_comment_within_multiline_comment
    assert_equal(<<CSS, render(<<SCSS))
body {
  /*
  //comment here
  */ }
CSS
body {
  /*
  //comment here
  */
}
SCSS
  end

  def test_malformed_media
    render <<SCSS
@media {
  margin: 0;
}
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "@media ": expected media query (e.g. print, screen, print and screen), was "{"', e.message
    assert_equal 1, e.sass_line
  end

  private

  def assert_selector_parses(selector)
    assert_parses <<SCSS
#{selector} {
  a: b; }
SCSS
  end

  def render(scss, options = {})
    tree = Sass::SCSS::CssParser.new(scss, options[:filename], nil).parse
    tree.options = Sass::Engine::DEFAULT_OPTIONS.merge(options)
    tree.render
  end
end
