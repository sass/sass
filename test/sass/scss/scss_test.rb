#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/test_helper'

class ScssTest < Test::Unit::TestCase
  include ScssTestHelper

  ## Script

  def test_variables
    assert_equal <<CSS, render(<<SCSS)
blat {
  a: foo; }
CSS
!var = "foo";

blat {a = !var}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo {
  a: 2; }
CSS
foo {
  !var = 2;
  a = !var; }
SCSS
  end

  def test_guard_assign
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: 1; }
CSS
!var = 1;
!var ||= 2;

foo {a = !var}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo {
  a: 2; }
CSS
!var ||= 2;

foo {a = !var}
SCSS
  end

  def test_sass_script
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: 3;
  b: foobar;
  c: 12px; }
CSS
foo {
  a = 1 + 2;
  b = "foo" + "bar";
  c = floor(12.3px); }
SCSS
  end

  def test_debug_directive
    assert_warning "test_debug_directive_inline.scss:2 DEBUG: hello world!" do
      assert_equal <<CSS, render(<<SCSS)
foo {
  a: b; }

bar {
  c: d; }
CSS
foo {a: b}
@debug "hello world!";
bar {c: d}
SCSS
    end
  end

  def test_for_directive
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: 1;
  a: 2;
  a: 3;
  a: 4; }
CSS
.foo {
  @for !var from 1 to 5 {a = !var;}
}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: 1;
  a: 2;
  a: 3;
  a: 4;
  a: 5; }
CSS
.foo {
  @for !var from 1 through 5 {a = !var;}
}
SCSS
  end

  def test_if_directive
    assert_equal <<CSS, render(<<SCSS)
foo {
  a: b; }
CSS
@if "foo" == "foo" {foo {a: b}}
@if "foo" != "foo" {bar {a: b}}
SCSS

    assert_equal <<CSS, render(<<SCSS)
foo {
  a: b; }
CSS
@if "foo" == "foo" {foo {a: b}}
@if "foo" != "foo" {bar {a: b}}
SCSS
  end

  def test_while_directive
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: 1;
  a: 2;
  a: 3;
  a: 4; }
CSS
!i = 1;

.foo {
  @while !i != 5 {
    a = !i;
    !i = !i + 1;
  }
}
SCSS
  end

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

  ## Namespace Properties

  def test_namespace_properties
    assert_equal <<CSS, render(<<SCSS)
foo {
  bar: baz;
  bang-bip: 1px;
  bang-bop: bar; }
CSS
foo {
  bar: baz;
  bang: {
    bip: 1px;
    bop: bar;}}
SCSS
  end

  def test_nested_namespace_properties
    assert_equal <<CSS, render(<<SCSS)
foo {
  bar: baz;
  bang-bip: 1px;
  bang-bop: bar;
  bang-blat-baf: bort; }
CSS
foo {
  bar: baz;
  bang: {
    bip: 1px;
    bop: bar;
    blat:{baf:bort}}}
SCSS
  end

  ## Mixins

  def test_basic_mixins
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: b; }
CSS
@mixin foo {
  .foo {a: b}}

@include foo;
SCSS

    assert_equal <<CSS, render(<<SCSS)
bar {
  c: d; }
  bar .foo {
    a: b; }
CSS
@mixin foo {
  .foo {a: b}}

bar {
  @include foo;
  c: d; }
SCSS

    assert_equal <<CSS, render(<<SCSS)
bar {
  a: b;
  c: d; }
CSS
@mixin foo {a: b}

bar {
  @include foo;
  c: d; }
SCSS
  end

  def test_mixins_with_empty_args
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: b; }
CSS
@mixin foo() {a: b}

.foo {@include foo();}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: b; }
CSS
@mixin foo() {a: b}

.foo {@include foo;}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: b; }
CSS
@mixin foo {a: b}

.foo {@include foo();}
SCSS
  end

  def test_mixins_with_args
    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: bar; }
CSS
@mixin foo(!a) {a = !a}

.foo {@include foo("bar")}
SCSS

    assert_equal <<CSS, render(<<SCSS)
.foo {
  a: bar;
  b: 12px; }
CSS
@mixin foo(!a, !b) {
  a = !a;
  b = !b; }

.foo {@include foo("bar", 12px)}
SCSS
  end

  ## Errors

  def test_mixin_defs_only_at_toplevel
    render <<SCSS
foo {
  @mixin bar {a: b}}
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal "Mixins may only be defined at the root of a document.", e.message
    assert_equal 2, e.sass_line
  end

  def test_imports_only_at_toplevel
    render <<SCSS
foo {
  @import "foo.scss";}
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal "Import directives may only be used at the root of a document.", e.message
    assert_equal 2, e.sass_line
  end

  def test_rules_beneath_properties
    render <<SCSS
foo {
  bar: {
    baz {
      bang: bop }}}
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Illegal nesting: Only properties may be nested beneath properties.', e.message
    assert_equal 3, e.sass_line
  end

  def test_uses_property_exception_with_star_hack
    render <<SCSS
foo {
  *bar:baz <fail>; }
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "  *bar:baz ": expected ";", was "<fail>; }"', e.message
    assert_equal 2, e.sass_line
  end

  def test_uses_property_exception_with_space_after_name
    render <<SCSS
foo {
  bar: baz <fail>; }
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "  bar: baz ": expected ";", was "<fail>; }"', e.message
    assert_equal 2, e.sass_line
  end

  def test_uses_property_exception_with_non_identifier_after_name
    render <<SCSS
foo {
  bar:1px <fail>; }
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "  bar:1px ": expected ";", was "<fail>; }"', e.message
    assert_equal 2, e.sass_line
  end

  def test_uses_property_exception_with_equals
    render <<SCSS
foo {
  bar=<fail>; }
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal "Expected expression, was lt token.", e.message
    assert_equal 2, e.sass_line
  end

  def test_uses_property_exception_when_followed_by_open_bracket
    render <<SCSS
foo {
  bar:{baz: .fail} }
SCSS
    assert(false, "Expected syntax error")
  rescue Sass::SyntaxError => e
    assert_equal 'Invalid CSS after "  bar:{baz: ": expected expression (e.g. 1px, bold) or "{", was ".fail} }"', e.message
    assert_equal 2, e.sass_line
  end
end
