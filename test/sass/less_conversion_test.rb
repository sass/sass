#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

begin
require 'sass/less'

class LessConversionTest < Test::Unit::TestCase
  def test_variable_declarations
    assert_renders <<SCSS, <<LESS
$var1: 2px 3px;

$var2: $var1 + 7px;

foo {
  prop: $var1 $var2; }
SCSS
@var1: 2px 3px;
@var2: @var1 + 7px;

foo {prop: @var1 @var2}
LESS
  end

  def test_nested_variable_declarations
    assert_renders <<SCSS, <<LESS
.foo {
  $var: 2px;
  prop: $var; }
SCSS
.foo {
  @var: 2px;
  prop: @var; }
LESS
  end

  # Selectors

  def test_element_selector
    assert_renders <<SCSS, <<LESS
foo {
  a: b; }
SCSS
foo {a: b}
LESS
  end

  def test_class_selector
    assert_renders <<SCSS, <<LESS
.foo {
  a: b; }
SCSS
.foo {a: b}
LESS
  end

  def test_id_selector
    assert_renders <<SCSS, <<LESS
#foo {
  a: b; }
SCSS
#foo {a: b}
LESS
  end

  def test_pseudoclass_selector
    assert_renders <<SCSS, <<LESS
:foo {
  a: b; }
SCSS
:foo {a: b}
LESS
  end

  def test_pseudoelement_selector
    assert_renders <<SCSS, <<LESS
::foo {
  a: b; }
SCSS
::foo {a: b}
LESS
  end

  def test_simple_selector_sequence
    assert_renders <<SCSS, <<LESS
a.foo#bar[attr=val] {
  a: b; }
SCSS
a.foo#bar[attr=val] {a: b}
LESS
  end

  def test_descendant_selector
    assert_renders <<SCSS, <<LESS
.foo .bar {
  a: b; }
SCSS
.foo .bar {a: b}
LESS
  end

  def test_child_selector
    assert_renders <<SCSS, <<LESS
.foo > .bar {
  a: b; }
SCSS
.foo > .bar {a: b}
LESS
  end

  def test_adjacent_selector
    assert_renders <<SCSS, <<LESS
.foo + .bar {
  a: b; }
SCSS
.foo + .bar {a: b}
LESS
  end

  def test_pseudoclass_in_sequence
    assert_renders <<SCSS, <<LESS
.foo:bar {
  a: b; }
SCSS
.foo:bar {a: b}
LESS
  end

  def test_pseudoelement_in_sequence
    assert_renders <<SCSS, <<LESS
.foo::bar {
  a: b; }
SCSS
.foo::bar {a: b}
LESS
  end

  # Properties

  def test_space_separated_props
    assert_renders <<SCSS, <<LESS
foo {
  a: foo bar baz; }
SCSS
foo {a: foo bar baz}
LESS
  end

  def test_comma_separated_props
    assert_renders <<SCSS, <<LESS
foo {
  a: foo, bar, baz; }
SCSS
foo {a: foo, bar, baz}
LESS
  end

  def test_numbers
    assert_renders <<SCSS, <<LESS
foo {
  a: 1 2.3 -8 5px 3%; }
SCSS
foo {a: 1 2.3 -8 5px 3%}
LESS
  end

  def test_colors
    assert_renders <<SCSS, <<LESS
foo {
  a: red #abcdef blue; }
SCSS
foo {a: #f00 #abcdef blue}
LESS
  end

  def test_strings
    assert_renders <<SCSS, <<LESS
foo {
  a: "foo @var bar" "baz bang" "quote'quote" 'quote"quote'; }
SCSS
foo {a: "foo @var bar" 'baz bang' "quote'quote" 'quote"quote'}
LESS
  end

  def test_slash
    assert_renders <<SCSS, <<LESS
foo {
  a: small/8px 7em/8px;
  b: 8/4;
  c: (8 / 4); }
SCSS
foo {
  a: small/8px 7em/8px;
  b: 8/4;
  c: 8 / 4; }
LESS
  end

  def test_url
    assert_renders <<SCSS, <<LESS
foo {
  a: url("http://foobar.com/fizzle.html?foo=bar&bar=baz");
  b: url("http://foobar.com/fizzle.html?foo=bar&bar=baz");
  c: url("http://foobar.com/fizzle.html?foo=bar&bar=baz"); }
SCSS
foo {
  a: url(http://foobar.com/fizzle.html?foo=bar&bar=baz);
  b: url("http://foobar.com/fizzle.html?foo=bar&bar=baz");
  c: url('http://foobar.com/fizzle.html?foo=bar&bar=baz'); }
LESS
  end

  def test_functions
    assert_renders <<SCSS, <<LESS
foo {
  a: baz(12px) rgba(80, 70 120, 0.76);
  b: faz(1px + 3px) bang($var, #aaaaaa * 3); }
SCSS
foo {
  a: baz(12px) rgba(80, 70 120, 0.76);
  b: faz(1px + 3px) bang(@var, #aaa * 3); }
LESS
  end

  def test_alpha_function
    assert_renders <<SCSS, <<LESS
foo {
  a: alpha(opacity=2px);
  b: alpha(opacity = $var); }
SCSS
foo {
  a: alpha(opacity=2px);
  b: alpha(opacity=@var); }
LESS
  end

  def test_variables
    assert_renders <<SCSS, <<LESS
foo {
  a: $var1 $var-foo; }
SCSS
foo {a: @var1 @var-foo}
LESS
  end

  def test_operators
    assert_renders <<SCSS, <<LESS
foo {
  a: 1px + 2px;
  b: #bbaa88 - #aa1122;
  c: 5 * 3;
  d: (8 / 4); }
SCSS
foo {
  a: 1px + 2px;
  b: #ba8 - #a12;
  c: 5 * 3;
  d: 8 / 4; }
LESS
  end

  def test_operators_with_parens
    assert_renders <<SCSS, <<LESS
foo {
  a: 1px + 2px * 3;
  b: (1px - 2px) / 3; }
SCSS
foo {
  a: 1px + (2px * 3);
  b: (1px - (2px)) / 3; }
LESS
  end

  def test_unary_minus
    assert_renders <<SCSS, <<LESS
foo {
  a: 1px + -3px; }
SCSS
foo {a: 1px + (- 3px)}
LESS
  end

  # Nested Rules

  def test_single_nested_rule
    assert_renders <<SCSS, <<LESS
foo bar {
  a: b; }
SCSS
foo {bar {a: b}}
LESS
  end

  def test_single_nested_rule_with_props
    assert_renders <<SCSS, <<LESS
foo {
  bar {
    a: b; }
  c: d;
  e: f; }
SCSS
foo {
  bar {a: b}
  c: d;
  e: f; }
LESS
  end

  def test_two_nested_rules
    assert_renders <<SCSS, <<LESS
foo {
  bar {
    a: b; }
  baz {
    c: d; } }
SCSS
foo {
  bar {a: b}
  baz {c: d} }
LESS
  end

  def test_two_nested_rules_with_props
    assert_renders <<SCSS, <<LESS
foo {
  bar {
    a: b; }
  baz {
    c: d; }
  e: f;
  g: h; }
SCSS
foo {
  bar {a: b}
  baz {c: d}
  e: f;
  g: h; }
LESS
  end

  def test_nested_rules_with_combinators
    assert_renders <<SCSS, <<LESS
foo {
  > bar {
    a: b; }
  + baz {
    c: d; } }
SCSS
foo {
  > bar {a: b}
  + baz {c: d} }
LESS
  end

  # Mixins

  def test_class_inheritance
    assert_renders <<SCSS, <<LESS
.foo {
  a: b; }

.bar {
  @extend .foo; }
SCSS
.foo {a: b}
.bar {.foo;}
LESS
  end

  def test_multiple_class_inheritance
    assert_renders <<SCSS, <<LESS
.foo {
  a: b; }

.bar {
  c: d; }

.baz {
  @extend .foo;
  @extend .bar; }
SCSS
.foo {a: b}
.bar {c: d}
.baz {.foo, .bar;}
LESS
  end

  def test_abstract_mixin
    assert_renders <<SCSS, <<LESS
@mixin foo {
  a: b; }

.bar {
  @include foo; }
SCSS
.foo() {a: b}
.bar {.foo;}
LESS
  end

  def test_mixin_with_args
    assert_renders <<SCSS, <<LESS
@mixin foo($a: 2px, $b: red) {
  a: $a; }

.bar {
  @include foo; }
SCSS
.foo(@a: 2px, @b: #f00) {a: @a}
.bar {.foo;}
LESS

    assert_renders <<SCSS, <<LESS
@mixin foo($a: 2px, $b: red) {
  a: $a; }

.bar {
  @include foo(5px); }
SCSS
.foo(@a: 2px, @b: #f00) {a: @a}
.bar {.foo(5px);}
LESS
  end

  private

  def assert_renders(scss, less)
    assert_equal(scss, Less::Engine.new(less).to_tree.to_sass_tree.to_scss)
  end
end

rescue LoadError => e
puts "\nCouldn't require less, skipping some tests."
end
