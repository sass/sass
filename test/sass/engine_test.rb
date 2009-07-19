#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'
require 'stringio'

class SassEngineTest < Test::Unit::TestCase
  # A map of erroneous Sass documents to the error messages they should produce.
  # The error messages may be arrays;
  # if so, the second element should be the line number that should be reported for the error.
  # If this isn't provided, the tests will assume the line number should be the last line of the document.
  EXCEPTION_MAP = {
    "!a = 1 + " => 'Expected expression, was end of text.',
    "!a = 1 + 2 +" => 'Expected expression, was end of text.',
    "!a = 1 + 2 + %" => 'Expected expression, was mod token.',
    "!a = foo(\"bar\"" => 'Expected rparen token, was end of text.',
    "!a = 1 }" => 'Unexpected end_interpolation token.',
    "!a = 1 }foo\"" => 'Unexpected end_interpolation token.',
    ":" => 'Invalid property: ":".',
    ": a" => 'Invalid property: ": a".',
    ":= a" => 'Invalid property: ":= a".',
    "a\n  :b" => 'Invalid property: ":b " (no value).',
    "a\n  b:" => 'Invalid property: "b: " (no value).',
    "a\n  :b: c" => 'Invalid property: ":b: c".',
    "a\n  :b:c d" => 'Invalid property: ":b:c d".',
    "a\n  :b=c d" => 'Invalid property: ":b=c d".',
    "a\n  :b c;" => 'Invalid property: ":b c;" (no ";" required at end-of-line).',
    "a\n  b: c;" => 'Invalid property: "b: c;" (no ";" required at end-of-line).',
    "a\n  b : c" => 'Invalid property: "b : c".',
    "a\n  b=c: d" => 'Invalid property: "b=c: d".',
    ":a" => 'Properties aren\'t allowed at the root of a document.',
    "!" => 'Invalid variable: "!".',
    "!a" => 'Invalid variable: "!a".',
    "! a" => 'Invalid variable: "! a".',
    "!a b" => 'Invalid variable: "!a b".',
    "!a = 1b + 2c" => "Incompatible units: 'c' and 'b'.",
    "!a = 1b < 2c" => "Incompatible units: 'c' and 'b'.",
    "!a = 1b > 2c" => "Incompatible units: 'c' and 'b'.",
    "!a = 1b <= 2c" => "Incompatible units: 'c' and 'b'.",
    "!a = 1b >= 2c" => "Incompatible units: 'c' and 'b'.",
    "a\n  :b= 1b * 2c" => "2b*c isn't a valid CSS value.",
    "a\n  :b= 1b % 2c" => "Cannot modulo by a number with units: 2c.",
    "!a = 2px + #ccc" => "Cannot add a number with units (2px) to a color (#cccccc).",
    "!a = #ccc + 2px" => "Cannot add a number with units (2px) to a color (#cccccc).",
    "& a\n  :b c" => ["Base-level rules cannot contain the parent-selector-referencing character '&'.", 1],
    "a\n  :b\n    c" => "Illegal nesting: Only properties may be nested beneath properties.",
    "a,\n  :b c" => ["Rules can\'t end in commas.", 1],
    "a," => "Rules can\'t end in commas.",
    "a,\n!b = 1" => ["Rules can\'t end in commas.", 1],
    "!a = b\n  :c d\n" => "Illegal nesting: Nothing may be nested beneath variable declarations.",
    "@import foo.sass" => "File to import not found or unreadable: foo.sass.",
    "@import templates/basic\n  foo" => "Illegal nesting: Nothing may be nested beneath import directives.",
    "foo\n  @import templates/basic" => "Import directives may only be used at the root of a document.",
    "foo\n  @import #{File.dirname(__FILE__)}/templates/basic" => "Import directives may only be used at the root of a document.",
    %Q{!foo = "bar" "baz" !} => %Q{Syntax error in '"bar" "baz" !' at character 20.},
    "=foo\n  :color red\n.bar\n  +bang" => "Undefined mixin 'bang'.",
    ".bar\n  =foo\n    :color red\n" => ["Mixins may only be defined at the root of a document.", 2],
    "=foo\n  :color red\n.bar\n  +foo\n    :color red" => "Illegal nesting: Nothing may be nested beneath mixin directives.",
    "    a\n  b: c" => ["Indenting at the beginning of the document is illegal.", 1],
    " \n   \n\t\n  a\n  b: c" => ["Indenting at the beginning of the document is illegal.", 4],
    "a\n  b: c\n b: c" => ["Inconsistent indentation: 1 space was used for indentation, but the rest of the document was indented using 2 spaces.", 3],
    "a\n  b: c\na\n b: c" => ["Inconsistent indentation: 1 space was used for indentation, but the rest of the document was indented using 2 spaces.", 4],
    "a\n\t\tb: c\n\tb: c" => ["Inconsistent indentation: 1 tab was used for indentation, but the rest of the document was indented using 2 tabs.", 3],
    "a\n  b: c\n   b: c" => ["Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces.", 3],
    "a\n  b: c\n  a\n   d: e" => ["Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces.", 4],
    "a\n  b: c\na\n    d: e" => ["The line was indented 2 levels deeper than the previous line.", 4],
    "a\n  b: c\n  a\n        d: e" => ["The line was indented 3 levels deeper than the previous line.", 4],
    "a\n \tb: c" => ["Indentation can't use both tabs and spaces.", 2],
    "=a(" => 'Expected rparen token, was end of text.',
    "=a(b)" => 'Expected rparen token, was ident token.',
    "=a(,)" => "Expected rparen token, was comma token.",
    "=a(!)" => "Syntax error in '(!)' at character 4.",
    "=a(!foo bar)" => "Expected rparen token, was ident token.",
    "=foo\n  bar: baz\n+foo" => ["Properties aren't allowed at the root of a document.", 2],
    "a-\#{!b\n  c: d" => ["Expected end_interpolation token, was end of text.", 1],
    "=a(!b = 1, !c)" => "Required argument !c must come before any optional arguments.",
    "=a(!b = 1)\n  :a= !b\ndiv\n  +a(1,2)" => "Mixin a takes 1 argument but 2 were passed.",
    "=a(!b)\n  :a= !b\ndiv\n  +a" => "Mixin a is missing parameter !b.",
    "@else\n  a\n    b: c" => ["@else must come after @if.", 1],
    "@if false\n@else foo" => "Invalid else directive '@else foo': expected 'if <expr>'.",
    "@if false\n@else if " => "Invalid else directive '@else if': expected 'if <expr>'.",
    "a\n  !b = 12\nc\n  d = !b" => 'Undefined variable: "!b".',
    "=foo\n  !b = 12\nc\n  +foo\n  d = !b" => 'Undefined variable: "!b".',
    '@for !a from "foo" to 1' => '"foo" is not an integer.',
    '@for !a from 1 to "2"' => '"2" is not an integer.',
    '@for !a from 1 to "foo"' => '"foo" is not an integer.',
    '@for !a from 1 to 1.232323' => '1.232 is not an integer.',
    '@for !a from 1px to 3em' => "Incompatible units: 'em' and 'px'.",
    '@if' => "Invalid if directive '@if': expected expression.",
    '@while' => "Invalid while directive '@while': expected expression.",
    '@debug' => "Invalid debug directive '@debug': expected expression.",

    # Regression tests
    "a\n  b:\n    c\n    d" => ["Illegal nesting: Only properties may be nested beneath properties.", 3],
    "& foo\n  bar: baz\n  blat: bang" => ["Base-level rules cannot contain the parent-selector-referencing character '&'.", 1],
    "a\n  b: c\n& foo\n  bar: baz\n  blat: bang" => ["Base-level rules cannot contain the parent-selector-referencing character '&'.", 3],
  }

  def teardown
    clean_up_sassc
  end

  def test_basic_render
    renders_correctly "basic", { :style => :compact }
  end

  def test_empty_render
    assert_equal "", render("")
  end

  def test_multiple_calls_to_render
    sass = Sass::Engine.new("a\n  b: c")
    assert_equal sass.render, sass.render
  end

  def test_alternate_styles
    renders_correctly "expanded", { :style => :expanded }
    renders_correctly "compact", { :style => :compact }
    renders_correctly "nested", { :style => :nested }
    renders_correctly "compressed", { :style => :compressed }
  end
  
  def test_flexible_tabulation
    assert_equal("p {\n  a: b; }\n  p q {\n    c: d; }\n",
                 render("p\n a: b\n q\n  c: d\n"))
    assert_equal("p {\n  a: b; }\n  p q {\n    c: d; }\n",
                 render("p\n\ta: b\n\tq\n\t\tc: d\n"))
  end
  
  EXCEPTION_MAP.each do |key, value|
    define_method("test_exception (#{key.inspect})") do
      line = 10
      begin
        silence_warnings {Sass::Engine.new(key, :filename => __FILE__, :line => line).render}
      rescue Sass::SyntaxError => err
        value = [value] unless value.is_a?(Array)

        assert_equal(value.first, err.message, "Line: #{key}")
        assert_equal(__FILE__, err.sass_filename)
        assert_equal((value[1] || key.split("\n").length) + line - 1, err.sass_line, "Line: #{key}")
        assert_match(/#{Regexp.escape(__FILE__)}:[0-9]+/, err.backtrace[0], "Line: #{key}")
      else
        assert(false, "Exception not raised for\n#{key}")
      end
    end
  end

  def test_exception_line
    to_render = <<SASS
rule
  :prop val
  // comment!

  :broken
SASS
    begin
      Sass::Engine.new(to_render).render
    rescue Sass::SyntaxError => err
      assert_equal(5, err.sass_line)
    else
      assert(false, "Exception not raised for '#{to_render}'!")
    end
  end

  def test_exception_location
    to_render = <<SASS
rule
  :prop val
  // comment!

  :broken
SASS
    begin
      Sass::Engine.new(to_render, :filename => __FILE__, :line => (__LINE__-7)).render
    rescue Sass::SyntaxError => err
      assert_equal(__FILE__, err.sass_filename)
      assert_equal((__LINE__-6), err.sass_line)
    else
      assert(false, "Exception not raised for '#{to_render}'!")
    end
  end

  def test_imported_exception
    [nil, 2].each do |i|
      begin
        Sass::Engine.new("@import bork#{i}", :load_paths => [File.dirname(__FILE__) + '/templates/']).render
      rescue Sass::SyntaxError => err
        assert_equal(2, err.sass_line)
        assert_match(/bork#{i}\.sass$/, err.sass_filename)
      else
        assert(false, "Exception not raised for imported template: bork#{i}")
      end
    end
  end

  def test_css_import
    assert_equal("@import url(./fonts.css) screen;\n", render("@import url(./fonts.css) screen"))
    assert_equal("@import \"./fonts.css\" screen;\n", render("@import \"./fonts.css\" screen"))
  end

  def test_sass_import
    assert !File.exists?(sassc_path("importee"))
    renders_correctly "import", { :style => :compact, :load_paths => [File.dirname(__FILE__) + "/templates"] }
    assert File.exists?(sassc_path("importee"))
  end

  def test_no_cache
    assert !File.exists?(sassc_path("importee"))
    renders_correctly("import", {
        :style => :compact, :cache => false,
        :load_paths => [File.dirname(__FILE__) + "/templates"],
      })
    assert !File.exists?(sassc_path("importee"))
  end

  def test_units
    renders_correctly "units"
  end

  def test_default_function
    assert_equal("foo {\n  bar: url(foo.png); }\n", render(%Q{foo\n  bar = url("foo.png")\n}));
    assert_equal("foo {\n  bar: url(); }\n", render("foo\n  bar = url()\n"));
  end

  def test_string_minus
    assert_equal("foo {\n  bar: baz-boom-bat; }\n", render(%Q{foo\n  bar = "baz"-"boom"-"bat"}))
    assert_equal("foo {\n  bar: -baz-boom; }\n", render(%Q{foo\n  bar = -"baz"-"boom"}))
  end

  def test_string_div
    assert_equal("foo {\n  bar: baz/boom/bat; }\n", render(%Q{foo\n  bar = "baz"/"boom"/"bat"}))
    assert_equal("foo {\n  bar: /baz/boom; }\n", render(%Q{foo\n  bar = /"baz"/"boom"}))
  end

  def test_basic_multiline_selector
    assert_equal("#foo #bar,\n#baz #boom {\n  foo: bar; }\n",
                 render("#foo #bar,\n#baz #boom\n  :foo bar"))
    assert_equal("#foo #bar,\n#foo #baz {\n  foo: bar; }\n",
                 render("#foo\n  #bar,\n  #baz\n    :foo bar"))
    assert_equal("#foo,\n#bar {\n  foo: bar; }\n  #foo #baz,\n  #bar #baz {\n    foo: bar; }\n",
                 render("#foo,\n#bar\n  :foo bar\n  #baz\n    :foo bar"))
    assert_equal("#foo #bar, #baz #boom { foo: bar; }\n",
                 render("#foo #bar,\n#baz #boom\n  :foo bar", :style => :compact))
                 
    assert_equal("#foo #bar,#baz #boom{foo:bar}\n",
                 render("#foo #bar,\n#baz #boom\n  :foo bar", :style => :compressed))
  end

  def test_complex_multiline_selector
    renders_correctly "multiline"
  end

  def test_colon_only
    begin
      render("a\n  b: c", :property_syntax => :old)
    rescue Sass::SyntaxError => e
      assert_equal("Illegal property syntax: can't use new syntax when :property_syntax => :old is set.",
                   e.message)
    else
      assert(false, "SyntaxError not raised for :property_syntax => :old")
    end

    begin
      render("a\n  :b c", :property_syntax => :new)
    rescue Sass::SyntaxError => e
      assert_equal("Illegal property syntax: can't use old syntax when :property_syntax => :new is set.",
                   e.message)
    else
      assert(false, "SyntaxError not raised for :property_syntax => :new")
    end
  end

  def test_pseudo_elements
    assert_equal(<<CSS, render(<<SASS))
::first-line {
  size: 10em; }
CSS
::first-line
  size: 10em
SASS
  end

  def test_directive
    assert_equal("@a b;\n", render("@a b"))

    assert_equal("@a {\n  b: c; }\n", render("@a\n  :b c"))
    assert_equal("@a { b: c; }\n", render("@a\n  :b c", :style => :compact))
    assert_equal("@a {\n  b: c;\n}\n", render("@a\n  :b c", :style => :expanded))
    assert_equal("@a{b:c}\n", render("@a\n  :b c", :style => :compressed))

    assert_equal("@a {\n  b: c;\n  d: e; }\n",
                 render("@a\n  :b c\n  :d e"))
    assert_equal("@a { b: c; d: e; }\n",
                 render("@a\n  :b c\n  :d e", :style => :compact))
    assert_equal("@a {\n  b: c;\n  d: e;\n}\n",
                 render("@a\n  :b c\n  :d e", :style => :expanded))
    assert_equal("@a{b:c;d:e}\n",
                 render("@a\n  :b c\n  :d e", :style => :compressed))

    assert_equal("@a {\n  #b {\n    c: d; } }\n",
                 render("@a\n  #b\n    :c d"))
    assert_equal("@a { #b { c: d; } }\n",
                 render("@a\n  #b\n    :c d", :style => :compact))
    assert_equal("@a {\n  #b {\n    c: d;\n  }\n}\n",
                 render("@a\n  #b\n    :c d", :style => :expanded))
    assert_equal("@a{#b{c:d}}\n",
                 render("@a\n  #b\n    :c d", :style => :compressed))

    assert_equal("@a {\n  #b {\n    a: b; }\n    #b #c {\n      d: e; } }\n",
                 render("@a\n  #b\n    :a b\n    #c\n      :d e"))
    assert_equal("@a { #b { a: b; }\n  #b #c { d: e; } }\n",
                 render("@a\n  #b\n    :a b\n    #c\n      :d e", :style => :compact))
    assert_equal("@a {\n  #b {\n    a: b;\n  }\n  #b #c {\n    d: e;\n  }\n}\n",
                 render("@a\n  #b\n    :a b\n    #c\n      :d e", :style => :expanded))
    assert_equal("@a{#b{a:b}#b #c{d:e}}\n",
                 render("@a\n  #b\n    :a b\n    #c\n      :d e", :style => :compressed))
                 
    assert_equal("@a {\n  #foo,\n  #bar {\n    b: c; } }\n",
                 render("@a\n  #foo, \n  #bar\n    :b c"))
    assert_equal("@a { #foo, #bar { b: c; } }\n",
                 render("@a\n  #foo, \n  #bar\n    :b c", :style => :compact))
    assert_equal("@a {\n  #foo,\n  #bar {\n    b: c;\n  }\n}\n",
                 render("@a\n  #foo, \n  #bar\n    :b c", :style => :expanded))
    assert_equal("@a{#foo,#bar{b:c}}\n",
                 render("@a\n  #foo, \n  #bar\n    :b c", :style => :compressed))

    to_render = <<END
@a
  :b c
  #d
    :e f
  :g h
END
    rendered = <<END
@a { b: c;
  #d { e: f; }
  g: h; }
END
    assert_equal(rendered, render(to_render, :style => :compact))
    
    assert_equal("@a{b:c;#d{e:f}g:h}\n", render(to_render, :style => :compressed))
  end

  def test_line_annotations
    assert_equal(<<CSS, render(<<SASS, :line_comments => true, :style => :compact))
/* line 2, test_line_annotations_inline.sass */
foo bar { foo: bar; }
/* line 5, test_line_annotations_inline.sass */
foo baz { blip: blop; }

/* line 9, test_line_annotations_inline.sass */
floodle { flop: blop; }

/* line 18, test_line_annotations_inline.sass */
bup { mix: on; }
/* line 15, test_line_annotations_inline.sass */
bup mixin { moop: mup; }

/* line 22, test_line_annotations_inline.sass */
bip hop, skip hop { a: b; }
CSS
foo
  bar
    foo: bar

  baz
    blip: blop


floodle

  flop: blop

=mxn
  mix: on
  mixin
    moop: mup

bup
  +mxn

bip, skip
  hop
    a: b
SASS
  end

  def test_line_annotations_with_filename
    renders_correctly "line_numbers", :line_comments => true, :load_paths => [File.dirname(__FILE__) + "/templates"]
  end

  def test_empty_first_line
    assert_equal("#a {\n  b: c; }\n", render("#a\n\n  b: c"))
  end

  def test_escaped_rule
    assert_equal(":focus {\n  a: b; }\n", render("\\:focus\n  a: b"))
    assert_equal("a {\n  b: c; }\n  a :focus {\n    d: e; }\n", render("\\a\n  b: c\n  \\:focus\n    d: e"))
  end

  def test_cr_newline
    assert_equal("foo {\n  a: b;\n  c: d;\n  e: f; }\n", render("foo\r  a: b\r\n  c: d\n\r  e: f"))
  end

  def test_or_eq
    assert_equal("foo {\n  a: b; }\n", render(%Q{!foo = "b"\n!foo ||= "c"\nfoo\n  a = !foo}))
    assert_equal("foo {\n  a: b; }\n", render(%Q{!foo ||= "b"\nfoo\n  a = !foo}))
  end
  
  def test_mixins
    renders_correctly "mixins", { :style => :expanded }
  end

  def test_mixins_dont_interfere_with_sibling_combinator
    assert_equal("foo + bar {\n  a: b; }\nfoo + baz {\n  c: d; }\n",
                 render("foo\n  +\n    bar\n      a: b\n    baz\n      c: d"))
  end

  def test_mixin_args
    assert_equal("blat {\n  baz: hi; }\n", render(<<SASS))
=foo(!bar)
  baz = !bar
blat
  +foo(\"hi\")
SASS
    assert_equal("blat {\n  baz: 3; }\n", render(<<SASS))
=foo(!a, !b)
  baz = !a + !b
blat
  +foo(1, 2)
SASS
    assert_equal("blat {\n  baz: 4;\n  baz: 3;\n  baz: 5;\n  bang: 3; }\n", render(<<SASS))
=foo(!c = (6 + 4) / 2)
  baz = !c
!c = 3
blat
  +foo(!c + 1)
  +foo((!c + 3)/2)
  +foo
  bang = !c
SASS
  end

  def test_default_values_for_mixin_arguments
    assert_equal("white {\n  color: white; }\n\nblack {\n  color: black; }\n", render(<<SASS))
=foo(!a = #FFF)
  :color= !a
white
  +foo
black
  +foo(#000)
SASS
    assert_equal(<<CSS, render(<<SASS))
one {
  color: white;
  padding: 1px;
  margin: 4px; }

two {
  color: white;
  padding: 2px;
  margin: 5px; }

three {
  color: white;
  padding: 2px;
  margin: 3px; }
CSS
!a = 5px
=foo(!a, !b = 1px, !c = 3px + !b)
  :color= !a
  :padding= !b
  :margin= !c
one
  +foo(#fff)
two
  +foo(#fff, 2px)
three
  +foo(#fff, 2px, 3px)
SASS
  end

  def test_interpolation
    assert_equal("a-1 {\n  b-2-3: c-3; }\n", render(<<SASS))
!a = 1
!b = 2
!c = 3
a-\#{!a}
  b-\#{!b}-\#{!c}: c-\#{!a + !b}
SASS
  end

  def test_if_directive
    assert_equal("a {\n  b: 1; }\n", render(<<SASS))
!var = true
a
  @if !var
    b: 1
  @if not !var
    b: 2
SASS
  end

  def test_for
    assert_equal(<<CSS, render(<<SASS))
a-0 {
  2i: 0; }

a-1 {
  2i: 2; }

a-2 {
  2i: 4; }

a-3 {
  2i: 6; }

b-1 {
  j-1: 0; }

b-2 {
  j-1: 1; }

b-3 {
  j-1: 2; }

b-4 {
  j-1: 3; }
CSS
!a = 3
@for !i from 0 to !a + 1
  a-\#{!i}
    2i = 2 * !i

@for !j from 1 through 4
  b-\#{!j}
    j-1 = !j - 1
SASS
  end

  def test_while
    assert_equal(<<CSS, render(<<SASS))
a-5 {
  blooble: gloop; }

a-4 {
  blooble: gloop; }

a-3 {
  blooble: gloop; }

a-2 {
  blooble: gloop; }

a-1 {
  blooble: gloop; }
CSS
!a = 5
@while !a != 0
  a-\#{!a}
    blooble: gloop
  !a = !a - 1
SASS
  end

  def test_else
    assert_equal(<<CSS, render(<<SASS))
a {
  t1: t;
  t2: t;
  t3: t;
  t4: t; }
CSS
a
  @if true
    t1: t
  @else
    f1: f

  @if false
    f2: f
  @else
    t2: t

  @if false
    f3: f1
  @else if 1 + 1 == 3
    f3: f2
  @else
    t3: t

  @if false
    f4: f1
  @else if 1 + 1 == 2
    t4: t
  @else
    f4: f2

  @if false
    f5: f1
  @else if false
    f5: f2
SASS
  end

  def test_variable_reassignment
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 1;
  c: 2; }
CSS
!a = 1
a
  b = !a
  !a = 2
  c = !a
SASS
  end

  def test_variable_scope
    assert_equal(<<CSS, render(<<SASS))
a {
  b-1: c;
  b-2: c;
  d: 12; }

b {
  d: 17; }
CSS
!i = 12
a
  @for !i from 1 through 2
    b-\#{!i}: c
  d = !i

=foo
  !i = 17

b
  +foo
  d = !i
SASS
  end

  def test_argument_error
    assert_raise(Sass::SyntaxError) { render("a\n  b = hsl(1)") }
  end

  def test_comments_at_the_top_of_a_document
    render(<<SASS)
//
  This is a comment that
  continues to the second line.
foo
  bar: baz
SASS
  end

  def test_loud_comments_containing_a_comment_close
    actual_css = render(<<SASS)
/*
  This is a comment that
  continues to the second line. */
foo
  bar: baz
SASS
assert_equal(<<CSS, actual_css)
/* This is a comment that
 * continues to the second line. */
foo {
  bar: baz; }
CSS
  end

  def test_quoted_colon
    assert_equal(<<CSS, render(<<SASS))
a b[foo="bar: baz"] {
  c: d; }
CSS
a
  b[foo="bar: baz"]
    c: d
SASS
  end

  def test_quoted_comma
    assert_equal(<<CSS, render(<<SASS))
a b[foo="bar, baz"] {
  c: d; }
CSS
a
  b[foo="bar, baz"]
    c: d
SASS
  end

  def test_quoted_ampersand
    assert_equal(<<CSS, render(<<SASS))
a b[foo="bar & baz"] {
  c: d; }
CSS
a
  b[foo="bar & baz"]
    c: d
SASS
  end

  def test_empty_selector_warning
    assert_warning(<<END) {render("foo bar")}
WARNING:
Selector "foo bar" doesn't have any properties and will not be rendered.
END

    assert_warning(<<END) {render(<<SASS)}
WARNING:
Selector
  foo, bar, baz,
  bang, bip, bop
doesn't have any properties and will not be rendered.
END
foo, bar, baz,
bang, bip, bop
SASS
  end

  # Regression tests

  def test_parens_in_mixins
    assert_equal(<<CSS, render(<<SASS))
.foo {
  color: #01ff7f;
  background-color: #000102; }
CSS
=foo(!c1, !c2 = rgb(0, 1, 2))
  color = !c1
  background-color = !c2

.foo
  +foo(rgb(1,255,127))
SASS
  end

  def test_comment_beneath_prop
    assert_equal(<<RESULT, render(<<SOURCE))
.box {
  border-style: solid; }
RESULT
.box
  :border
    //:color black
    :style solid
SOURCE

    assert_equal(<<RESULT, render(<<SOURCE))
.box {
  /* :color black */
  border-style: solid; }
RESULT
.box
  :border
    /*:color black
    :style solid
SOURCE

    assert_equal(<<RESULT, render(<<SOURCE, :style => :compressed))
.box{border-style:solid}
RESULT
.box
  :border
    /*:color black
    :style solid
SOURCE
  end

  def test_compressed_comment_beneath_directive
    assert_equal(<<RESULT, render(<<SOURCE, :style => :compressed))
@foo{a:b}
RESULT
@foo
  a: b
  /*b: c
SOURCE
  end

  def test_comment_with_crazy_indentation
    assert_equal(<<CSS, render(<<SASS))
/* This is a loud comment:
 *          Where the indentation is wonky. */
.comment {
  width: 1px; }
CSS
/*
  This is a loud comment:
           Where the indentation is wonky.
//
  This is a silent comment:
           Where the indentation is wonky.
.comment
  width: 1px
SASS
  end

  def test_plus_with_space
    assert_equal(<<CSS, render(<<SASS))
a + b {
  color: green; }
CSS
a
  + b
    color: green
SASS
  end

  def test_empty_line_comment
    assert_equal(<<CSS, render(<<SASS))
/* Foo
 *
 * Bar */
CSS
/*
  Foo

  Bar
SASS
  end

  def test_empty_comment
    assert_equal(<<CSS, render(<<SASS))
/* */
a {
  /* */
  b: c; }
CSS
/*
a
  /*
  b: c
SASS
  end

  private
  
  def render(sass, options = {})
    munge_filename options
    Sass::Engine.new(sass, options).render
  end

  def renders_correctly(name, options={})
    sass_file  = load_file(name, "sass")
    css_file   = load_file(name, "css")
    options[:filename] ||= filename(name, "sass")
    options[:css_filename] ||= filename(name, "css")
    css_result = Sass::Engine.new(sass_file, options).render
    assert_equal css_file, css_result
  end

  def load_file(name, type = "sass")
    @result = ''
    File.new(filename(name, type)).each_line { |l| @result += l }
    @result
  end

  def filename(name, type)
    File.dirname(__FILE__) + "/#{type == 'sass' ? 'templates' : 'results'}/#{name}.#{type}"
  end

  def sassc_path(template)
    sassc_path = File.join(File.dirname(__FILE__) + "/templates/#{template}.sass")
    Sass::Files.send(:sassc_filename, sassc_path, Sass::Engine::DEFAULT_OPTIONS)
  end
end
