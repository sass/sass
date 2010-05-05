#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

begin
require 'sass/less'

class LessConversionTest < Test::Unit::TestCase
  def test_variable_declarations
    assert_renders <<SCSS, <<LESS
$var1: 2px 3px;
$var2: $var1 + 7px;

$var3: fizz;

foo {
  prop: $var1 $var2 $var3; }
SCSS
@var1: 2px 3px;
@var2: @var1 + 7px;

@var3: fizz;

foo {prop: @var1 @var2 @var3}
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

  def test_import
    path = relative_path_to(File.dirname(__FILE__) + "/templates/importee.less")
    resolved_path = relative_path_to(File.dirname(__FILE__) + "/templates/importee")
    assert_renders <<SCSS, <<LESS
@import "#{resolved_path}";
@import "#{resolved_path}";

@import "#{resolved_path}";
@import "#{resolved_path}";
@import "#{resolved_path}";
SCSS
@import url(#{path});
@import url("#{path}");

@import url('#{path}');
@import '#{path}';
@import "#{path}";
LESS
  end

  def test_mixins_found_through_import
    path = relative_path_to(File.dirname(__FILE__) + "/templates/importee.less")
    resolved_path = relative_path_to(File.dirname(__FILE__) + "/templates/importee")
    assert_renders <<SCSS, <<LESS
@import "#{resolved_path}";

.baz {
  @extend .foo;
  @include bar; }
SCSS
@import "#{path}";

.baz {.foo; .bar;}
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

  def test_comma_selector
    assert_renders <<SCSS, <<LESS
foo, .bar .baz, :bang {
  a: b; }
SCSS
foo, .bar .baz, :bang {a: b}
LESS
  end

  def test_nested_comma_selector
    assert_renders <<SCSS, <<LESS
foo bar, .baz {
  .bang, &:bip bap {
    a: b; } }
SCSS
foo bar, .baz {
  .bang, :bip bap {a: b} }
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

  def test_operator_precedence
    assert_renders <<SCSS, <<LESS
foo {
  a: 1 + 2 * 3 + 4;
  b: 1 * 2 + 3 * 4;
  c: 1 - 2 + 2 - 4;
  d: 1 + 2 - 3 + 4;
  e: 1 / 2 - 3 / 4;
  f: 1 - 2 / 3 - 4;
  g: 1 / 2 * 3 / 4; }
SCSS
foo {
  a: 1 + 2 * 3 + 4;
  b: 1 * 2 + 3 * 4;
  c: 1 - 2 + 2 - 4;
  d: 1 + 2 - 3 + 4;
  e: 1 / 2 - 3 / 4;
  f: 1 - 2 / 3 - 4;
  g: 1 / 2 * 3 / 4; }
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

  def test_nested_pseudo_rules
    assert_renders <<SCSS, <<LESS
foo {
  &:bar {
    a: b; }
  &::baz {
    c: d; } }
SCSS
foo {
  :bar {a: b}
  ::baz {c: d} }
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

  def test_pseudoclass_inheritance
    assert_renders <<SCSS, <<LESS
:foo {
  a: b; }

:bar {
  @extend :foo; }
SCSS
:foo {a: b}
:bar {:foo;}
LESS
  end

  def test_multiple_pseudoclass_inheritance
    assert_renders <<SCSS, <<LESS
:foo:bar {
  a: b; }

:baz {
  @extend :foo:bar; }
SCSS
:foo:bar {a: b}
:baz {:foo:bar;}
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
@mixin foo($a: 2px + 3px, $b: red) {
  a: $a; }

.bar {
  @include foo(5px); }
SCSS
.foo(@a: 2px + 3px, @b: #f00) {a: @a}
.bar {.foo(5px);}
LESS
  end

  ## Disallowed Mixins

  def test_nested_mixin
    assert_warning(<<WARN) {assert_renders <<SCSS, <<LESS}
WARNING: Sass doesn't support mixing in selector sequences.
Replacing ".foo .bar" with "@extend .bar"
WARN
.foo .bar {
  a: b; }

.bar {
  // .foo .bar;
  @extend .bar; }
SCSS
.foo .bar {a: b}
.bar {.foo .bar;}
LESS
  end

  def test_child_selector_mixin
    assert_warning(<<WARN) {assert_renders <<SCSS, <<LESS}
WARNING: Sass doesn't support mixing in selector sequences.
Replacing "> .bar" with "@extend .bar"
WARN
.foo > .bar {
  a: b; }

.bar {
  // > .bar;
  @extend .bar; }
SCSS
.foo > .bar {a: b}
.bar {> .bar;}
LESS
  end

  # Accessors

  def test_property_accessor
    assert_warning(<<WARN) {assert_renders <<SCSS, <<LESS}
WARNING: Sass doesn't support attribute accessors.
Ignoring .magic-box['content']
WARN
.magic-box {
  content: "gold"; }

.foo {
  content: /* .magic-box['content'] */; }
SCSS
.magic-box {content: "gold"}
.foo {content: .magic-box['content']}
LESS
  end

  def test_variable_accessor
    assert_warning(<<WARN) {assert_renders <<SCSS, <<LESS}
WARNING: Sass doesn't support attribute accessors.
Ignoring .magic-box[@content]
WARN
.magic-box {
  $content: "gold";
  content: $content; }

.foo {
  content: /* .magic-box[@content] */; }
SCSS
.magic-box {@content: "gold"; content: @content}
.foo {content: .magic-box[@content]}
LESS
  end

  private

  def assert_renders(scss, less)
    assert_equal(scss, Less::Engine.new(less).to_tree.to_sass_tree.to_scss)
  end

  # Necessary because Less can't import absolute files
  def relative_path_to(file)
    file = Pathname.new(file)
    pwd = file.absolute? ? Dir.pwd : "."
    file.relative_path_from(Pathname.new(pwd))
  end
end

rescue LoadError => e
puts "\nCouldn't require less, skipping some tests."
end
