#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require 'test_helper'
require 'sass/test_helper'
require 'sass/engine'
require 'stringio'
require 'mock_importer'
require 'pathname'

module Sass::Script::Functions::UserFunctions
  def option(name)
    Sass::Script::String.new(@options[name.value.to_sym].to_s)
  end
end

class SassEngineTest < Test::Unit::TestCase
  FAKE_FILE_NAME = __FILE__.gsub(/rb$/,"sass")
  # A map of erroneous Sass documents to the error messages they should produce.
  # The error messages may be arrays;
  # if so, the second element should be the line number that should be reported for the error.
  # If this isn't provided, the tests will assume the line number should be the last line of the document.
  EXCEPTION_MAP = {
    "$a: 1 + " => 'Invalid CSS after "1 +": expected expression (e.g. 1px, bold), was ""',
    "$a: 1 + 2 +" => 'Invalid CSS after "1 + 2 +": expected expression (e.g. 1px, bold), was ""',
    "$a: 1 + 2 + %" => 'Invalid CSS after "1 + 2 + ": expected expression (e.g. 1px, bold), was "%"',
    "$a: foo(\"bar\"" => 'Invalid CSS after "foo("bar"": expected ")", was ""',
    "$a: 1 }" => 'Invalid CSS after "1 ": expected expression (e.g. 1px, bold), was "}"',
    "$a: 1 }foo\"" => 'Invalid CSS after "1 ": expected expression (e.g. 1px, bold), was "}foo""',
    ":" => 'Invalid property: ":".',
    ": a" => 'Invalid property: ": a".',
    "a\n  :b" => <<MSG,
Invalid property: ":b" (no value).
If ":b" should be a selector, use "\\:b" instead.
MSG
    "a\n  b:" => 'Invalid property: "b:" (no value).',
    "a\n  :b: c" => 'Invalid property: ":b: c".',
    "a\n  :b:c d" => 'Invalid property: ":b:c d".',
    "a\n  :b c;" => 'Invalid CSS after "c": expected expression (e.g. 1px, bold), was ";"',
    "a\n  b: c;" => 'Invalid CSS after "c": expected expression (e.g. 1px, bold), was ";"',
    ".foo ^bar\n  a: b" => ['Invalid CSS after ".foo ": expected selector, was "^bar"', 1],
    "a\n  @extend .foo ^bar" => 'Invalid CSS after ".foo ": expected selector, was "^bar"',
    "a: b" => 'Properties are only allowed within rules, directives, or other properties.',
    ":a b" => 'Properties are only allowed within rules, directives, or other properties.',
    "$" => 'Invalid variable: "$".',
    "$a" => 'Invalid variable: "$a".',
    "$ a" => 'Invalid variable: "$ a".',
    "$a b" => 'Invalid variable: "$a b".',
    "$a: 1b + 2c" => "Incompatible units: 'c' and 'b'.",
    "$a: 1b < 2c" => "Incompatible units: 'c' and 'b'.",
    "$a: 1b > 2c" => "Incompatible units: 'c' and 'b'.",
    "$a: 1b <= 2c" => "Incompatible units: 'c' and 'b'.",
    "$a: 1b >= 2c" => "Incompatible units: 'c' and 'b'.",
    "a\n  b: 1b * 2c" => "2b*c isn't a valid CSS value.",
    "a\n  b: 1b % 2c" => "Cannot modulo by a number with units: 2c.",
    "$a: 2px + #ccc" => "Cannot add a number with units (2px) to a color (#cccccc).",
    "$a: #ccc + 2px" => "Cannot add a number with units (2px) to a color (#cccccc).",
    "& a\n  :b c" => ["Base-level rules cannot contain the parent-selector-referencing character '&'.", 1],
    "a\n  :b\n    c" => "Illegal nesting: Only properties may be nested beneath properties.",
    "$a: b\n  :c d\n" => "Illegal nesting: Nothing may be nested beneath variable declarations.",
    "$a: b\n  :c d\n" => "Illegal nesting: Nothing may be nested beneath variable declarations.",
    "@import templates/basic\n  foo" => "Illegal nesting: Nothing may be nested beneath import directives.",
    "foo\n  @import foo.css" => "CSS import directives may only be used at the root of a document.",
    "@if true\n  @import foo" => "Import directives may not be used within control directives or mixins.",
    "@mixin foo\n  @import foo" => "Import directives may not be used within control directives or mixins.",
    "@import foo;" => "Invalid @import: expected end of line, was \";\".",
    '$foo: "bar" "baz" !' => %Q{Invalid CSS after ""bar" "baz" ": expected expression (e.g. 1px, bold), was "!"},
    '$foo: "bar" "baz" $' => %Q{Invalid CSS after ""bar" "baz" ": expected expression (e.g. 1px, bold), was "$"},
    "=foo\n  :color red\n.bar\n  +bang" => "Undefined mixin 'bang'.",
    "=foo\n  :color red\n.bar\n  +bang_bop" => "Undefined mixin 'bang_bop'.",
    "=foo\n  :color red\n.bar\n  +bang-bop" => "Undefined mixin 'bang-bop'.",
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
    "=a(" => 'Invalid CSS after "(": expected variable (e.g. $foo), was ""',
    "=a(b)" => 'Invalid CSS after "(": expected variable (e.g. $foo), was "b)"',
    "=a(,)" => 'Invalid CSS after "(": expected variable (e.g. $foo), was ",)"',
    "=a($)" => 'Invalid CSS after "(": expected variable (e.g. $foo), was "$)"',
    "=a($foo bar)" => 'Invalid CSS after "($foo ": expected ")", was "bar)"',
    "=foo\n  bar: baz\n+foo" => ["Properties are only allowed within rules, directives, or other properties.", 2],
    "a-\#{$b\n  c: d" => ['Invalid CSS after "a-#{$b": expected "}", was ""', 1],
    "=a($b: 1, $c)" => "Required argument $c must come before any optional arguments.",
    "=a($b: 1)\n  a: $b\ndiv\n  +a(1,2)" => "Mixin a takes 1 argument but 2 were passed.",
    "=a($b: 1)\n  a: $b\ndiv\n  +a(1,$c: 3)" => "Mixin a doesn't have an argument named $c",
    "=a($b)\n  a: $b\ndiv\n  +a" => "Mixin a is missing parameter $b.",
    "@function foo()\n  1 + 2" => "Functions can only contain variable declarations and control directives.",
    "@function foo()\n  foo: bar" => "Functions can only contain variable declarations and control directives.",
    "@function foo()\n  foo: bar\n  @return 3" => ["Functions can only contain variable declarations and control directives.", 2],
    "@function foo\n  @return 1" => ['Invalid CSS after "": expected "(", was ""', 1],
    "@function foo(\n  @return 1" => ['Invalid CSS after "(": expected variable (e.g. $foo), was ""', 1],
    "@function foo(b)\n  @return 1" => ['Invalid CSS after "(": expected variable (e.g. $foo), was "b)"', 1],
    "@function foo(,)\n  @return 1" => ['Invalid CSS after "(": expected variable (e.g. $foo), was ",)"', 1],
    "@function foo($)\n  @return 1" => ['Invalid CSS after "(": expected variable (e.g. $foo), was "$)"', 1],
    "@function foo()\n  @return" => 'Invalid @return: expected expression.',
    "@function foo()\n  @return 1\n    $var: val" => 'Illegal nesting: Nothing may be nested beneath return directives.',
    "foo\n  @function bar()\n    @return 1" => ['Functions may only be defined at the root of a document.', 2],
    "@function foo($a)\n  @return 1\na\n  b: foo()" => 'Function foo is missing parameter $a.',
    "@function foo()\n  @return 1\na\n  b: foo(2)" => 'Wrong number of arguments (1 for 0) for `foo\'',
    "@return 1" => '@return may only be used within a function.',
    "@if true\n  @return 1" => '@return may only be used within a function.',
    "@mixin foo\n  @return 1\n@include foo" => ['@return may only be used within a function.', 2],
    "@else\n  a\n    b: c" => ["@else must come after @if.", 1],
    "@if false\n@else foo" => "Invalid else directive '@else foo': expected 'if <expr>'.",
    "@if false\n@else if " => "Invalid else directive '@else if': expected 'if <expr>'.",
    "a\n  $b: 12\nc\n  d: $b" => 'Undefined variable: "$b".',
    "=foo\n  $b: 12\nc\n  +foo\n  d: $b" => 'Undefined variable: "$b".',
    "c\n  d: $b-foo" => 'Undefined variable: "$b-foo".',
    "c\n  d: $b_foo" => 'Undefined variable: "$b_foo".',
    '@for $a from "foo" to 1' => '"foo" is not an integer.',
    '@for $a from 1 to "2"' => '"2" is not an integer.',
    '@for $a from 1 to "foo"' => '"foo" is not an integer.',
    '@for $a from 1 to 1.232323' => '1.232 is not an integer.',
    '@for $a from 1px to 3em' => "Incompatible units: 'em' and 'px'.",
    '@if' => "Invalid if directive '@if': expected expression.",
    '@while' => "Invalid while directive '@while': expected expression.",
    '@debug' => "Invalid debug directive '@debug': expected expression.",
    %Q{@debug "a message"\n  "nested message"} => "Illegal nesting: Nothing may be nested beneath debug directives.",
    '@warn' => "Invalid warn directive '@warn': expected expression.",
    %Q{@warn "a message"\n  "nested message"} => "Illegal nesting: Nothing may be nested beneath warn directives.",
    "/* foo\n    bar\n  baz" => "Inconsistent indentation: previous line was indented by 4 spaces, but this line was indented by 2 spaces.",
    '+foo(1 + 1: 2)' => 'Invalid CSS after "(1 + 1": expected comma, was ": 2)"',
    '+foo($var: )' => 'Invalid CSS after "($var: ": expected mixin argument, was ")"',
    '+foo($var: a, $var: b)' => 'Keyword argument "$var" passed more than once',
    '+foo($var-var: a, $var_var: b)' => 'Keyword argument "$var-var" passed more than once',
    '+foo($var_var: a, $var-var: b)' => 'Keyword argument "$var_var" passed more than once',
    "a\n  b: foo(1 + 1: 2)" => 'Invalid CSS after "foo(1 + 1": expected comma, was ": 2)"',
    "a\n  b: foo($var: )" => 'Invalid CSS after "foo($var: ": expected function argument, was ")"',
    "a\n  b: foo($var: a, $var: b)" => 'Keyword argument "$var" passed more than once',
    "a\n  b: foo($var-var: a, $var_var: b)" => 'Keyword argument "$var-var" passed more than once',
    "a\n  b: foo($var_var: a, $var-var: b)" => 'Keyword argument "$var_var" passed more than once',
    "@if foo\n  @extend .bar" => ["Extend directives may only be used within rules.", 2],
    "$var: true\n@while $var\n  @extend .bar\n  $var: false" => ["Extend directives may only be used within rules.", 3],
    "@for $i from 0 to 1\n  @extend .bar" => ["Extend directives may only be used within rules.", 2],
    "@mixin foo\n  @extend .bar\n@include foo" => ["Extend directives may only be used within rules.", 2],

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

  def test_compile
    assert_equal "div { hello: world; }\n", Sass.compile("$who: world\ndiv\n  hello: $who", :syntax => :sass, :style => :compact)
    assert_equal "div { hello: world; }\n", Sass.compile("$who: world; div { hello: $who }", :style => :compact)
  end

  def test_compile_file
    FileUtils.mkdir_p(absolutize("tmp"))
    open(absolutize("tmp/test_compile_file.sass"), "w") {|f| f.write("$who: world\ndiv\n  hello: $who")}
    open(absolutize("tmp/test_compile_file.scss"), "w") {|f| f.write("$who: world; div { hello: $who }")}
    assert_equal "div { hello: world; }\n", Sass.compile_file(absolutize("tmp/test_compile_file.sass"), :style => :compact)
    assert_equal "div { hello: world; }\n", Sass.compile_file(absolutize("tmp/test_compile_file.scss"), :style => :compact)
  ensure
    FileUtils.rm_rf(absolutize("tmp"))
  end

  def test_compile_file_to_css_file
    FileUtils.mkdir_p(absolutize("tmp"))
    open(absolutize("tmp/test_compile_file.sass"), "w") {|f| f.write("$who: world\ndiv\n  hello: $who")}
    open(absolutize("tmp/test_compile_file.scss"), "w") {|f| f.write("$who: world; div { hello: $who }")}
    Sass.compile_file(absolutize("tmp/test_compile_file.sass"), absolutize("tmp/test_compile_file_sass.css"), :style => :compact)
    Sass.compile_file(absolutize("tmp/test_compile_file.scss"), absolutize("tmp/test_compile_file_scss.css"), :style => :compact)
    assert_equal "div { hello: world; }\n", File.read(absolutize("tmp/test_compile_file_sass.css"))
    assert_equal "div { hello: world; }\n", File.read(absolutize("tmp/test_compile_file_scss.css"))
  ensure
    FileUtils.rm_rf(absolutize("tmp"))
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
        silence_warnings {Sass::Engine.new(key, :filename => FAKE_FILE_NAME, :line => line).render}
      rescue Sass::SyntaxError => err
        value = [value] unless value.is_a?(Array)

        assert_equal(value.first.rstrip, err.message, "Line: #{key}")
        assert_equal(FAKE_FILE_NAME, err.sass_filename)
        assert_equal((value[1] || key.split("\n").length) + line - 1, err.sass_line, "Line: #{key}")
        assert_match(/#{Regexp.escape(FAKE_FILE_NAME)}:[0-9]+/, err.backtrace[0], "Line: #{key}")
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
      Sass::Engine.new(to_render, :filename => FAKE_FILE_NAME, :line => (__LINE__-7)).render
    rescue Sass::SyntaxError => err
      assert_equal(FAKE_FILE_NAME, err.sass_filename)
      assert_equal((__LINE__-6), err.sass_line)
    else
      assert(false, "Exception not raised for '#{to_render}'!")
    end
  end

  def test_imported_exception
    [1, 2, 3, 4, 5].each do |i|
      begin
        Sass::Engine.new("@import bork#{i}", :load_paths => [File.dirname(__FILE__) + '/templates/']).render
      rescue Sass::SyntaxError => err
        assert_equal(2, err.sass_line)
        assert_match(/(\/|^)bork#{i}\.sass$/, err.sass_filename)

        assert_hash_has(err.sass_backtrace.first,
          :filename => err.sass_filename, :line => err.sass_line)

        assert_nil(err.sass_backtrace[1][:filename])
        assert_equal(1, err.sass_backtrace[1][:line])

        assert_match(/(\/|^)bork#{i}\.sass:2$/, err.backtrace.first)
        assert_equal("(sass):1", err.backtrace[1])
      else
        assert(false, "Exception not raised for imported template: bork#{i}")
      end
    end
  end

  def test_double_imported_exception
    [1, 2, 3, 4, 5].each do |i|
      begin
        Sass::Engine.new("@import nested_bork#{i}", :load_paths => [File.dirname(__FILE__) + '/templates/']).render
      rescue Sass::SyntaxError => err
        assert_equal(2, err.sass_line)
        assert_match(/(\/|^)bork#{i}\.sass$/, err.sass_filename)

        assert_hash_has(err.sass_backtrace.first,
          :filename => err.sass_filename, :line => err.sass_line)

        assert_match(/(\/|^)nested_bork#{i}\.sass$/, err.sass_backtrace[1][:filename])
        assert_equal(2, err.sass_backtrace[1][:line])

        assert_nil(err.sass_backtrace[2][:filename])
        assert_equal(1, err.sass_backtrace[2][:line])

        assert_match(/(\/|^)bork#{i}\.sass:2$/, err.backtrace.first)
        assert_match(/(\/|^)nested_bork#{i}\.sass:2$/, err.backtrace[1])
        assert_equal("(sass):1", err.backtrace[2])
      else
        assert(false, "Exception not raised for imported template: bork#{i}")
      end
    end
  end

  def test_selector_tracing
    actual_css = render(<<-SCSS, :syntax => :scss, :trace_selectors => true)
      @mixin mixed {
        .mixed { color: red; }
      }
      .context {
        @include mixed;
      }
    SCSS
    assert_equal(<<CSS,actual_css)
/* on line 2 of test_selector_tracing_inline.scss, in `mixed'
   from line 5 of test_selector_tracing_inline.scss */
.context .mixed {
  color: red; }
CSS
  end

  def test_mixin_exception
    render(<<SASS)
=error-mixin($a)
  color: $a * 1em * 1px

=outer-mixin($a)
  +error-mixin($a)

.error
  +outer-mixin(12)
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_equal(2, err.sass_line)
    assert_equal(filename_for_test, err.sass_filename)
    assert_equal("error-mixin", err.sass_mixin)

    assert_hash_has(err.sass_backtrace.first, :line => err.sass_line,
      :filename => err.sass_filename, :mixin => err.sass_mixin)
    assert_hash_has(err.sass_backtrace[1], :line => 5,
      :filename => filename_for_test, :mixin => "outer-mixin")
    assert_hash_has(err.sass_backtrace[2], :line => 8,
      :filename => filename_for_test, :mixin => nil)

    assert_equal("#{filename_for_test}:2:in `error-mixin'", err.backtrace.first)
    assert_equal("#{filename_for_test}:5:in `outer-mixin'", err.backtrace[1])
    assert_equal("#{filename_for_test}:8", err.backtrace[2])
  end

  def test_mixin_callsite_exception
    render(<<SASS)
=one-arg-mixin($a)
  color: $a

=outer-mixin($a)
  +one-arg-mixin($a, 12)

.error
  +outer-mixin(12)
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_hash_has(err.sass_backtrace.first, :line => 5,
      :filename => filename_for_test, :mixin => "one-arg-mixin")
    assert_hash_has(err.sass_backtrace[1], :line => 5,
      :filename => filename_for_test, :mixin => "outer-mixin")
    assert_hash_has(err.sass_backtrace[2], :line => 8,
      :filename => filename_for_test, :mixin => nil)
  end

  def test_mixin_exception_cssize
    render(<<SASS)
=parent-ref-mixin
  & foo
    a: b

=outer-mixin
  +parent-ref-mixin

+outer-mixin
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_hash_has(err.sass_backtrace.first, :line => 2,
      :filename => filename_for_test, :mixin => "parent-ref-mixin")
    assert_hash_has(err.sass_backtrace[1], :line => 6,
      :filename => filename_for_test, :mixin => "outer-mixin")
    assert_hash_has(err.sass_backtrace[2], :line => 8,
      :filename => filename_for_test, :mixin => nil)
  end

  def test_mixin_and_import_exception
    Sass::Engine.new("@import nested_mixin_bork", :load_paths => [File.dirname(__FILE__) + '/templates/']).render
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_match(/(\/|^)nested_mixin_bork\.sass$/, err.sass_backtrace.first[:filename])
    assert_hash_has(err.sass_backtrace.first, :mixin => "error-mixin", :line => 4)

    assert_match(/(\/|^)mixin_bork\.sass$/, err.sass_backtrace[1][:filename])
    assert_hash_has(err.sass_backtrace[1], :mixin => "outer-mixin", :line => 2)

    assert_match(/(\/|^)mixin_bork\.sass$/, err.sass_backtrace[2][:filename])
    assert_hash_has(err.sass_backtrace[2], :mixin => nil, :line => 5)

    assert_match(/(\/|^)nested_mixin_bork\.sass$/, err.sass_backtrace[3][:filename])
    assert_hash_has(err.sass_backtrace[3], :mixin => nil, :line => 6)

    assert_hash_has(err.sass_backtrace[4], :filename => nil, :mixin => nil, :line => 1)
  end

  def test_basic_mixin_loop_exception
    render <<SASS
@mixin foo
  @include foo
@include foo
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_equal("An @include loop has been found: foo includes itself", err.message)
    assert_hash_has(err.sass_backtrace[0], :mixin => "foo", :line => 2)
  end

  def test_double_mixin_loop_exception
    render <<SASS
@mixin foo
  @include bar
@mixin bar
  @include foo
@include foo
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_equal(<<MESSAGE.rstrip, err.message)
An @include loop has been found:
    foo includes bar
    bar includes foo
MESSAGE
    assert_hash_has(err.sass_backtrace[0], :mixin => "bar", :line => 4)
    assert_hash_has(err.sass_backtrace[1], :mixin => "foo", :line => 2)
  end

  def test_deep_mixin_loop_exception
    render <<SASS
@mixin foo
  @include bar

@mixin bar
  @include baz

@mixin baz
  @include foo

@include foo
SASS
    assert(false, "Exception not raised")
  rescue Sass::SyntaxError => err
    assert_equal(<<MESSAGE.rstrip, err.message)
An @include loop has been found:
    foo includes bar
    bar includes baz
    baz includes foo
MESSAGE
    assert_hash_has(err.sass_backtrace[0], :mixin => "baz", :line => 8)
    assert_hash_has(err.sass_backtrace[1], :mixin => "bar", :line => 5)
    assert_hash_has(err.sass_backtrace[2], :mixin => "foo", :line => 2)
  end

  def test_exception_css_with_offset
    opts = {:full_exception => true, :line => 362}
    render(("a\n  b: c\n" * 10) + "d\n  e:\n" + ("f\n  g: h\n" * 10), opts)
  rescue Sass::SyntaxError => e
    assert_equal(<<CSS, Sass::SyntaxError.exception_to_css(e, opts).split("\n")[0..15].join("\n"))
/*
Syntax error: Invalid property: "e:" (no value).
        on line 383 of test_exception_css_with_offset_inline.sass

378: a
379:   b: c
380: a
381:   b: c
382: d
383:   e:
384: f
385:   g: h
386: f
387:   g: h
388: f
CSS
  else
    assert(false, "Exception not raised for test_exception_css_with_offset")
  end

  def test_exception_css_with_mixins
    opts = {:full_exception => true}
    render(<<SASS, opts)
=error-mixin($a)
  color: $a * 1em * 1px

=outer-mixin($a)
  +error-mixin($a)

.error
  +outer-mixin(12)
SASS
  rescue Sass::SyntaxError => e
    assert_equal(<<CSS, Sass::SyntaxError.exception_to_css(e, opts).split("\n")[0..13].join("\n"))
/*
Syntax error: 12em*px isn't a valid CSS value.
        on line 2 of test_exception_css_with_mixins_inline.sass, in `error-mixin'
        from line 5 of test_exception_css_with_mixins_inline.sass, in `outer-mixin'
        from line 8 of test_exception_css_with_mixins_inline.sass

1: =error-mixin($a)
2:   color: $a * 1em * 1px
3: 
4: =outer-mixin($a)
5:   +error-mixin($a)
6: 
7: .error
CSS
  else
    assert(false, "Exception not raised")
  end

  def test_cssize_exception_css
    opts = {:full_exception => true}
    render(<<SASS, opts)
.filler
  stuff: "stuff!"

a: b

.more.filler
  a: b
SASS
  rescue Sass::SyntaxError => e
    assert_equal(<<CSS, Sass::SyntaxError.exception_to_css(e, opts).split("\n")[0..11].join("\n"))
/*
Syntax error: Properties are only allowed within rules, directives, or other properties.
        on line 4 of test_cssize_exception_css_inline.sass

1: .filler
2:   stuff: "stuff!"
3: 
4: a: b
5: 
6: .more.filler
7:   a: b
CSS
  else
    assert(false, "Exception not raised")
  end

  def test_css_import
    assert_equal("@import url(./fonts.css);\n", render("@import \"./fonts.css\""))
  end

  def test_http_import
    assert_equal("@import url(http://fonts.googleapis.com/css?family=Droid+Sans);\n",
      render("@import \"http://fonts.googleapis.com/css?family=Droid+Sans\""))
  end

  def test_http_import_with_interpolation
    assert_equal("@import url(http://fonts.googleapis.com/css?family=Droid+Sans);\n",
      render("$family: unquote(\"Droid+Sans\")\n@import \"http://fonts.googleapis.com/css?family=\#{$family}\"\n"))
    assert_equal("@import url(\"http://fonts.googleapis.com/css?family=Droid+Sans\");\n",
      render("$family: unquote(\"Droid+Sans\")\n@import url(\"http://fonts.googleapis.com/css?family=\#{$family}\")\n"))
  end

  def test_url_import
    assert_equal("@import url(fonts.sass);\n", render("@import url(fonts.sass)"))
  end

  def test_sass_import
    sassc_file = sassc_path("importee")
    assert !File.exists?(sassc_file)
    renders_correctly "import", { :style => :compact, :load_paths => [File.dirname(__FILE__) + "/templates"] }
    assert File.exists?(sassc_file)
  end

  def test_sass_pathname_import
    sassc_file = sassc_path("importee")
    assert !File.exists?(sassc_file)
    renders_correctly("import",
      :style => :compact,
      :load_paths => [Pathname.new(File.dirname(__FILE__) + "/templates")])
    assert File.exists?(sassc_file)
  end

  def test_nonexistent_import
    assert_raise_message(Sass::SyntaxError, <<ERR.rstrip) do
File to import not found or unreadable: nonexistent.sass.
Load path: #{Dir.pwd}
ERR
      render("@import nonexistent.sass")
    end
  end

  def test_nonexistent_extensionless_import
    assert_raise_message(Sass::SyntaxError, <<ERR.rstrip) do
File to import not found or unreadable: nonexistent.
Load path: #{Dir.pwd}
ERR
      render("@import nonexistent")
    end
  end

  def test_no_cache
    assert !File.exists?(sassc_path("importee"))
    renders_correctly("import", {
        :style => :compact, :cache => false,
        :load_paths => [File.dirname(__FILE__) + "/templates"],
      })
    assert !File.exists?(sassc_path("importee"))
  end

  def test_import_in_rule
    assert_equal(<<CSS, render(<<SASS, :load_paths => [File.dirname(__FILE__) + '/templates/']))
.foo #foo {
  background-color: #bbaaff; }

.bar {
  a: b; }
  .bar #foo {
    background-color: #bbaaff; }
CSS
.foo
  @import partial

.bar
  a: b
  @import partial
SASS
  end

  def test_nested_import_with_toplevel_constructs
    Sass::Engine.new(".foo\n  @import importee", :load_paths => [File.dirname(__FILE__) + '/templates/']).render
  rescue Sass::SyntaxError => err
    assert_equal(3, err.sass_line)
    assert_match(/(\/|^)importee\.sass$/, err.sass_filename)

    assert_hash_has(err.sass_backtrace.first,
      :filename => err.sass_filename, :line => err.sass_line)

    assert_nil(err.sass_backtrace[1][:filename])
    assert_equal(2, err.sass_backtrace[1][:line])

    assert_match(/(\/|^)importee\.sass:3$/, err.backtrace.first)
    assert_equal("(sass):2", err.backtrace[1])
  else
    assert(false, "Exception not raised for importing mixins nested")
  end

  def test_units
    renders_correctly "units"
  end

  def test_default_function
    assert_equal(<<CSS, render(<<SASS))
foo {
  bar: url("foo.png"); }
CSS
foo
  bar: url("foo.png")
SASS
    assert_equal("foo {\n  bar: url(); }\n", render("foo\n  bar: url()\n"));
  end

  def test_string_minus
    assert_equal("foo {\n  bar: baz-boom-bat; }\n", render(%Q{foo\n  bar: baz-boom-bat}))
    assert_equal("foo {\n  bar: -baz-boom; }\n", render(%Q{foo\n  bar: -baz-boom}))
  end

  def test_string_div
    assert_equal("foo {\n  bar: baz/boom/bat; }\n", render(%Q{foo\n  bar: baz/boom/bat}))
    assert_equal("foo {\n  bar: /baz/boom; }\n", render(%Q{foo\n  bar: /baz/boom}))
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

    assert_equal("#foo #bar,\n#baz #boom {\n  foo: bar; }\n",
                 render("#foo #bar,,\n,#baz #boom,\n  :foo bar"))

    assert_equal("#bip #bop {\n  foo: bar; }\n",
                 render("#bip #bop,, ,\n  :foo bar"))
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
      assert_equal(2, e.sass_line)
    else
      assert(false, "SyntaxError not raised for :property_syntax => :old")
    end

    begin
      render("a\n  :b c", :property_syntax => :new)
      assert_equal(2, e.sass_line)
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

  def test_property_hacks
    assert_equal(<<CSS, render(<<SASS))
foo {
  _name: val;
  *name: val;
  #name: val;
  .name: val;
  name/**/: val;
  name/*\\**/: val;
  name: val; }
CSS
foo
  _name: val
  *name: val
  #name: val
  .name: val
  name/**/: val
  name/*\\**/: val
  name: val
SASS
  end

  def test_properties_with_space_after_colon
    assert_equal <<CSS, render(<<SASS)
foo {
  bar: baz;
  bizz: bap; }
CSS
foo
  bar : baz
  bizz	: bap
SASS
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

  def test_debug_info
    esc_file_name = Sass::SCSS::RX.escape_ident(Sass::Util.scope("test_debug_info_inline.sass"))

    assert_equal(<<CSS, render(<<SASS, :debug_info => true, :style => :compact))
@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\000032}}
foo bar { foo: bar; }
@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\000035}}
foo baz { blip: blop; }

@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\000039}}
floodle { flop: blop; }

@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\0000318}}
bup { mix: on; }
@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\0000315}}
bup mixin { moop: mup; }

@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\0000322}}
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

  def test_debug_info_without_filename
    assert_equal(<<CSS, Sass::Engine.new(<<SASS, :debug_info => true).render)
@media -sass-debug-info{filename{font-family:}line{font-family:\\000031}}
foo {
  a: b; }
CSS
foo
  a: b
SASS
  end

  def test_debug_info_with_compressed
    assert_equal(<<CSS, render(<<SASS, :debug_info => true, :style => :compressed))
foo{a:b}
CSS
foo
  a: b
SASS
  end

  def test_debug_info_with_line_annotations
    esc_file_name = Sass::SCSS::RX.escape_ident(Sass::Util.scope("test_debug_info_with_line_annotations_inline.sass"))

    assert_equal(<<CSS, render(<<SASS, :debug_info => true, :line_comments => true))
@media -sass-debug-info{filename{font-family:file\\:\\/\\/#{esc_file_name}}line{font-family:\\000031}}
foo {
  a: b; }
CSS
foo
  a: b
SASS
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

  def test_property_with_content_and_nested_props
    assert_equal(<<CSS, render(<<SASS))
foo {
  a: b;
    a-c: d;
      a-c-e: f; }
CSS
foo
  a: b
    c: d
      e: f
SASS

    assert_equal(<<CSS, render(<<SASS))
foo {
  a: b;
    a-c-e: f; }
CSS
foo
  a: b
    c:
      e: f
SASS
  end

  def test_guarded_assign
    assert_equal("foo {\n  a: b; }\n", render(%Q{$foo: b\n$foo: c !default\nfoo\n  a: $foo}))
    assert_equal("foo {\n  a: b; }\n", render(%Q{$foo: b !default\nfoo\n  a: $foo}))
  end
  
  def test_mixins
    renders_correctly "mixins", { :style => :expanded }
  end

  def test_directive_style_mixins
    assert_equal <<CSS, render(<<SASS)
bar {
  prop: baz; }
CSS
@mixin foo($arg)
  prop: $arg

bar
  @include foo(baz)
SASS
  end

  def test_mixins_dont_interfere_with_sibling_combinator
    assert_equal("foo + bar {\n  a: b; }\nfoo + baz {\n  c: d; }\n",
                 render("foo\n  +\n    bar\n      a: b\n    baz\n      c: d"))
  end

  def test_mixin_args
    assert_equal("blat {\n  baz: hi; }\n", render(<<SASS))
=foo($bar)
  baz: $bar
blat
  +foo(hi)
SASS
    assert_equal("blat {\n  baz: 3; }\n", render(<<SASS))
=foo($a, $b)
  baz: $a + $b
blat
  +foo(1, 2)
SASS
    assert_equal("blat {\n  baz: 4;\n  baz: 3;\n  baz: 5;\n  bang: 3; }\n", render(<<SASS))
=foo($c: (6 + 4) / 2)
  baz: $c
$c: 3
blat
  +foo($c + 1)
  +foo(($c + 3)/2)
  +foo
  bang: $c
SASS
  end

  def test_default_values_for_mixin_arguments
    assert_equal("white {\n  color: white; }\n\nblack {\n  color: black; }\n", render(<<SASS))
=foo($a: #FFF)
  :color $a
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
$a: 5px
=foo($a, $b: 1px, $c: 3px + $b)
  :color $a
  :padding $b
  :margin $c
one
  +foo(#fff)
two
  +foo(#fff, 2px)
three
  +foo(#fff, 2px, 3px)
SASS
  end

  def test_hyphen_underscore_insensitive_mixins
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 12;
  c: foo; }
CSS
=mixin-hyphen
  b: 12

=mixin_under
  c: foo

a
  +mixin_hyphen
  +mixin-under
SASS
  end

  def test_css_identifier_mixin
    assert_equal(<<CSS, render(<<SASS))
a {
  foo: 12; }
CSS
=\\{foo\\(12\\)($a)
  foo: $a

a
  +\\{foo\\(12\\)(12)
SASS
  end

  def test_basic_function
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: 3; }
CSS
@function foo()
  @return 1 + 2

bar
  a: foo()
SASS
  end

  def test_function_args
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: 3; }
CSS
@function plus($var1, $var2)
  @return $var1 + $var2

bar
  a: plus(1, 2)
SASS
  end

  def test_function_arg_default
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: 3; }
CSS
@function plus($var1, $var2: 2)
  @return $var1 + $var2

bar
  a: plus(1)
SASS
  end

  def test_function_arg_keyword
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: 1bar; }
CSS
@function plus($var1: 1, $var2: 2)
  @return $var1 + $var2

bar
  a: plus($var2: bar)
SASS
  end

  def test_function_with_if
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: foo;
  b: bar; }
CSS
@function my-if($cond, $val1, $val2)
  @if $cond
    @return $val1
  @else
    @return $val2

bar
  a: my-if(true, foo, bar)
  b: my-if(false, foo, bar)
SASS
  end

  def test_function_with_var
    assert_equal(<<CSS, render(<<SASS))
bar {
  a: 1; }
CSS
@function foo($val1, $val2)
  $intermediate: $val1 + $val2
  @return $intermediate/3

bar
  a: foo(1, 2)
SASS
  end

  def test_control_directive_in_nested_property
    assert_equal(<<CSS, render(<<SASS))
foo {
  a-b: c; }
CSS
foo
  a:
    @if true
      b: c
SASS
  end

  def test_interpolation
    assert_equal("a-1 {\n  b-2-3: c-3; }\n", render(<<SASS))
$a: 1
$b: 2
$c: 3
a-\#{$a}
  b-\#{$b}-\#{$c}: c-\#{$a + $b}
SASS
  end

  def test_complex_property_interpolation
    assert_equal(<<CSS, render(<<SASS))
a-1 {
  b-2 3-fizzap18: c-3; }
CSS
$a: 1
$b: 2
$c: 3
a-\#{$a}
  b-\#{$b $c}-\#{fizzap + ($c + 15)}: c-\#{$a + $b}
SASS
  end

  def test_if_directive
    assert_equal("a {\n  b: 1; }\n", render(<<SASS))
$var: true
a
  @if $var
    b: 1
  @if not $var
    b: 2
SASS
  end

  def test_for
    assert_equal(<<CSS, render(<<SASS))
a-0 {
  two-i: 0; }

a-1 {
  two-i: 2; }

a-2 {
  two-i: 4; }

a-3 {
  two-i: 6; }

b-1 {
  j-1: 0; }

b-2 {
  j-1: 1; }

b-3 {
  j-1: 2; }

b-4 {
  j-1: 3; }
CSS
$a: 3
@for $i from 0 to $a + 1
  a-\#{$i}
    two-i: 2 * $i

@for $j from 1 through 4
  b-\#{$j}
    j-1: $j - 1
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
$a: 5
@while $a != 0
  a-\#{$a}
    blooble: gloop
  $a: $a - 1
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

  def test_each
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 1px;
  b: 2px;
  b: 3px;
  b: 4px;
  c: foo;
  c: bar;
  c: baz;
  c: bang;
  d: blue; }
CSS
a
  @each $number in 1px 2px 3px 4px
    b: $number
  @each $str in foo, bar, baz, bang
    c: $str
  @each $single in blue
    d: $single
SASS
  end

  def test_variable_reassignment
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 1;
  c: 2; }
CSS
$a: 1
a
  b: $a
  $a: 2
  c: $a
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
$i: 12
a
  @for $i from 1 through 2
    b-\#{$i}: c
  d: $i

=foo
  $i: 17

b
  +foo
  d: $i
SASS
  end

  def test_hyphen_underscore_insensitive_variables
    assert_equal(<<CSS, render(<<SASS))
a {
  b: c; }

d {
  e: 13;
  f: foobar; }
CSS
$var-hyphen: 12
$var_under: foo

a
  $var_hyphen: 1 + $var_hyphen
  $var-under: $var-under + bar
  b: c

d
  e: $var-hyphen
  f: $var_under
SASS
  end

  def test_css_identifier_variable
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 12; }
CSS
$\\{foo\\(12\\): 12

a
  b: $\\{foo\\(12\\)
SASS
  end

  def test_important
    assert_equal(<<CSS, render(<<SASS))
a {
  b: 12px !important; }
CSS
$foo: 12px
a
  b: $foo !important
SASS
  end

  def test_argument_error
    assert_raise(Sass::SyntaxError) { render("a\n  b: hsl(1)") }
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

  def test_loud_comments_with_starred_lines
    assert_equal(<<CSS, render(<<SASS))
/* This is a comment that
 * continues to the second line.
 * And even to the third! */
CSS
/* This is a comment that
 * continues to the second line.
 * And even to the third!
SASS
  end

  def test_loud_comments_with_no_space_after_starred_lines
    assert_equal(<<CSS, render(<<SASS))
/*bip bop
 *beep boop
 *bap blimp */
CSS
/*bip bop
 *beep boop
 *bap blimp
SASS
  end

  def test_comment_indentation_at_beginning_of_doc
    assert_equal <<CSS, render(<<SASS)
/* foo
 * bar
 *   baz */
foo {
  a: b; }
CSS
/* foo
   bar
     baz
foo
  a: b
SASS
  end

  def test_unusual_comment_indentation
    assert_equal <<CSS, render(<<SASS)
foo {
  /* foo
   * bar
   *   baz */ }
CSS
foo
  /* foo
     bar
       baz
SASS
  end

  def test_loud_comment_with_close
    assert_equal <<CSS, render(<<SASS)
foo {
  /* foo
   * bar */ }
CSS
foo
  /* foo
     bar */
SASS
  end

  def test_loud_comment_with_separate_line_close
    assert_equal <<CSS, render(<<SASS)
foo {
  /* foo
   * bar
   */ }
CSS
foo
  /* foo
   * bar
   */
SASS
  end

  def test_loud_comment_in_compressed_mode
    assert_equal <<CSS, render(<<SASS, :style => :compressed)
foo{color:blue;/* foo
 * bar
 */}
CSS
foo
  color: blue
  /*! foo
   * bar
   */
SASS
  end

  def test_loud_comment_in_silent_comment
    silence_warnings {assert_equal <<CSS, render(<<SASS, :style => :compressed)}
foo{color:blue;/* foo */
/* bar */
/* bip */
/* baz */}
CSS
foo
  color: blue
  //! foo
  //! bar
  //!
    bip
    baz
SASS
  end

  def test_loud_comment_is_evaluated
    assert_equal <<CSS, render(<<SASS)
/* Hue: 327.216deg */
CSS
/*!
  Hue: \#{hue(#f836a0)}
SASS
  end

  def test_attribute_selector_with_spaces
    assert_equal(<<CSS, render(<<SASS))
a b[foo=bar] {
  c: d; }
CSS
a
  b[foo = bar]
    c: d
SASS
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
WARNING on line 1 of test_empty_selector_warning_inline.sass:
This selector doesn't have any properties and will not be rendered.
END
  end

  def test_root_level_pseudo_class_with_new_properties
    assert_equal(<<CSS, render(<<SASS, :property_syntax => :new))
:focus {
  outline: 0; }
CSS
:focus
  outline: 0
SASS
  end

  def test_pseudo_class_with_new_properties
    assert_equal(<<CSS, render(<<SASS, :property_syntax => :new))
p :focus {
  outline: 0; }
CSS
p
  :focus
    outline: 0
SASS
  end

  def test_nil_option
    assert_equal(<<CSS, render(<<SASS, :format => nil))
foo {
  a: b; }
CSS
foo
  a: b
SASS
  end

  def test_interpolation_in_raw_functions
    assert_equal(<<CSS, render(<<SASS))
foo {
  filter: progid:Microsoft.foo.bar.Baz(flip=foobar, bang=#00ff00cc); }
CSS
foo
  filter: progid:Microsoft.foo.bar.Baz(flip=\#{foo + bar}, bang=#00ff00cc)
SASS
  end

  # SassScript string behavior

  def test_plus_preserves_quotedness
    assert_equal(<<CSS, render(<<SASS))
foo {
  a: "foo1";
  b: "1foo";
  c: foo1;
  d: 1foo;
  e: "foobar";
  f: foobar; }
CSS
foo
  a: "foo" + 1
  b: 1 + "foo"
  c: foo + 1
  d: 1 + foo
  e: "foo" + bar
  f: foo + "bar"
SASS
  end

  def test_colon_properties_preserve_quotedness
    assert_equal(<<CSS, render(<<SASS))
foo {
  a: "foo";
  b: bar;
  c: "foo" bar;
  d: foo, "bar"; }
CSS
foo
  a: "foo"
  b: bar
  c: "foo" bar
  d: foo, "bar"
SASS
  end

  def test_colon_variables_preserve_quotedness
    assert_equal(<<CSS, render(<<SASS))
foo {
  a: "foo";
  b: bar; }
CSS
$a: "foo"
$b: bar

foo
  a: $a
  b: $b
SASS
  end

  def test_colon_args_preserve_quotedness
    assert_equal(<<CSS, render(<<SASS))
foo {
  a: "foo";
  b: bar;
  c: "foo" bar;
  d: foo, "bar"; }
CSS
=foo($a: "foo", $b: bar, $c: "foo" bar, $d: (foo, "bar"))
  foo
    a: $a
    b: $b
    c: $c
    d: $d

+foo
SASS
  end

  def test_interpolation_unquotes_strings
    assert_equal(<<CSS, render(<<SASS))
.foo-bar {
  a: b; }
CSS
.foo-\#{"bar"}
  a: b
SASS

    assert_equal(<<CSS, render(<<SASS))
.foo {
  a: b c; }
CSS
.foo
  a: b \#{"c"}
SASS
  end

  def test_interpolation_unquotes_strings_in_vars
    assert_equal(<<CSS, render(<<SASS))
.foo-bar {
  a: b; }
CSS
$var: "bar"

.foo-\#{$var}
  a: b
SASS
  end

  def test_interpolation_doesnt_deep_unquote_strings
    assert_equal(<<CSS, render(<<SASS))
.foo {
  a: "bar" "baz"; }
CSS
.foo
  a: \#{"bar" "baz"}
SASS
  end

  def test_warn_directive
  expected_warning = <<EXPECTATION
WARNING: this is a warning
         on line 4 of test_warn_directive_inline.sass

WARNING: this is a mixin warning
         on line 2 of test_warn_directive_inline.sass, in `foo'
         from line 7 of test_warn_directive_inline.sass
EXPECTATION
    assert_warning expected_warning do
      assert_equal <<CSS, render(<<SASS)
bar {
  c: d; }
CSS
=foo
  @warn "this is a mixin warning"

@warn "this is a warning"
bar
  c: d
  +foo
SASS
    end
  end

  def test_warn_directive_when_quiet
    assert_warning "" do
      assert_equal <<CSS, render(<<SASS, :quiet => true)
CSS
@warn "this is a warning"
SASS
    end
  end

  def test_warn_with_imports
    expected_warning = <<WARN
WARNING: In the main file
         on line 1 of #{File.dirname(__FILE__)}/templates/warn.sass

WARNING: Imported
         on line 1 of #{File.dirname(__FILE__)}/templates/warn_imported.sass
         from line 2 of #{File.dirname(__FILE__)}/templates/warn.sass

WARNING: In an imported mixin
         on line 4 of #{File.dirname(__FILE__)}/templates/warn_imported.sass, in `emits-a-warning'
         from line 3 of #{File.dirname(__FILE__)}/templates/warn.sass
WARN
    assert_warning expected_warning do
      renders_correctly "warn", :style => :compact, :load_paths => [File.dirname(__FILE__) + "/templates"]
    end
  end

  def test_media_bubbling
    assert_equal <<CSS, render(<<SASS)
.foo {
  a: b; }
  @media bar {
    .foo {
      c: d; } }
  .foo .baz {
    e: f; }
    @media bip {
      .foo .baz {
        g: h; } }

.other {
  i: j; }
CSS
.foo
  a: b
  @media bar
    c: d
  .baz
    e: f
    @media bip
      g: h

.other
  i: j
SASS

    assert_equal <<CSS, render(<<SASS, :style => :compact)
.foo { a: b; }
@media bar { .foo { c: d; } }
.foo .baz { e: f; }
@media bip { .foo .baz { g: h; } }

.other { i: j; }
CSS
.foo
  a: b
  @media bar
    c: d
  .baz
    e: f
    @media bip
      g: h

.other
  i: j
SASS

    assert_equal <<CSS, render(<<SASS, :style => :expanded)
.foo {
  a: b;
}
@media bar {
  .foo {
    c: d;
  }
}
.foo .baz {
  e: f;
}
@media bip {
  .foo .baz {
    g: h;
  }
}

.other {
  i: j;
}
CSS
.foo
  a: b
  @media bar
    c: d
  .baz
    e: f
    @media bip
      g: h

.other
  i: j
SASS
  end

  def test_double_media_bubbling
    assert_equal <<CSS, render(<<SASS)
@media bar and baz {
  .foo {
    c: d; } }
CSS
@media bar
  @media baz
    .foo
      c: d
SASS

    assert_equal <<CSS, render(<<SASS)
@media bar {
  .foo {
    a: b; } }
  @media bar and baz {
    .foo {
      c: d; } }
CSS
.foo
  @media bar
    a: b
    @media baz
      c: d
SASS
  end

  def test_rule_media_rule_bubbling
    assert_equal <<CSS, render(<<SASS)
@media bar {
  .foo {
    a: b;
    e: f; }
    .foo .baz {
      c: d; } }
CSS
.foo
  @media bar
    a: b
    .baz
      c: d
    e: f
SASS
  end

  def test_nested_media_around_properties
    assert_equal <<CSS, render(<<SASS)
.outside {
  color: red;
  background: blue; }
  @media print {
    .outside {
      color: black; } }
    @media print and nested {
      .outside .inside {
        border: 1px solid black; } }
  .outside .middle {
    display: block; }
CSS
.outside
  color: red
  @media print
    color: black
    .inside
      @media nested
        border: 1px solid black
  background: blue
  .middle
    display: block
SASS
  end

  def test_media_with_parent_references
    sass_str = <<SASS
.outside
  @media print
    &.inside
      border: 1px solid black
SASS
    css_str = <<CSS
@media print {
  .outside.inside {
    border: 1px solid black; } }
CSS
    assert_equal css_str, render(sass_str)
  end

  # Regression tests

  def test_interpolated_comment_in_mixin
    assert_equal <<CSS, render(<<SASS)
/* color: red */
.foo {
  color: red; }

/* color: blue */
.foo {
  color: blue; }

/* color: green */
.foo {
  color: green; }
CSS
=foo($var)
  /*! color: \#{$var}
  .foo
    color: $var

+foo(red)
+foo(blue)
+foo(green)
SASS
  end

  def test_parens_in_mixins
    assert_equal(<<CSS, render(<<SASS))
.foo {
  color: #01ff7f;
  background-color: #000102; }
CSS
=foo($c1, $c2: rgb(0, 1, 2))
  color: $c1
  background-color: $c2

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
    /* :color black
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

  def test_options_available_in_environment
    assert_equal(<<CSS, render(<<SASS))
a {
  b: nested; }
CSS
a
  b: option("style")
SASS
  end

  def test_mixin_no_arg_error
    assert_raise_message(Sass::SyntaxError, 'Invalid CSS after "($bar,": expected variable (e.g. $foo), was ")"') do
      render(<<SASS)
=foo($bar,)
  bip: bap
SASS
    end
  end

  def test_import_with_commas_in_url
    assert_equal <<CSS, render(<<SASS)
@import url(foo.css?bar,baz);
CSS
@import url(foo.css?bar,baz)
SASS
  end

  def test_silent_comment_in_prop_val_after_important
    assert_equal(<<CSS, render(<<SASS))
.advanced {
  display: none !important; }
CSS
.advanced
  display: none !important // yeah, yeah. it's not really a style anyway.
SASS
  end

  def test_mixin_with_keyword_args
    assert_equal <<CSS, render(<<SASS)
.mixed {
  required: foo;
  arg1: default-val1;
  arg2: non-default-val2; }
CSS
=a-mixin($required, $arg1: default-val1, $arg2: default-val2)
  required: $required
  arg1: $arg1
  arg2: $arg2
.mixed
  +a-mixin(foo, $arg2: non-default-val2)
SASS
  end

  def test_mixin_with_keyword_arg_variable_value
    assert_equal <<CSS, render(<<SASS)
.mixed {
  required: foo;
  arg1: default-val1;
  arg2: a-value; }
CSS
=a-mixin($required, $arg1: default-val1, $arg2: default-val2)
  required: $required
  arg1: $arg1
  arg2: $arg2
.mixed
  $a-value: a-value
  +a-mixin(foo, $arg2: $a-value)
SASS
  end

  def test_mixin_keyword_args_handle_variable_underscore_dash_equivalence
    assert_equal <<CSS, render(<<SASS)
.mixed {
  required: foo;
  arg1: non-default-val1;
  arg2: non-default-val2; }
CSS
=a-mixin($required, $arg-1: default-val1, $arg_2: default-val2)
  required: $required
  arg1: $arg_1
  arg2: $arg-2
.mixed
  +a-mixin(foo, $arg-2: non-default-val2, $arg_1: non-default-val1)
SASS
  end

  def test_passing_required_args_as_a_keyword_arg
    assert_equal <<CSS, render(<<SASS)
.mixed {
  required: foo;
  arg1: default-val1;
  arg2: default-val2; }
CSS
=a-mixin($required, $arg1: default-val1, $arg2: default-val2)
  required: $required
  arg1: $arg1
  arg2: $arg2
.mixed
  +a-mixin($required: foo)
SASS
  end

  def test_passing_all_as_keyword_args_in_opposite_order
    assert_equal <<CSS, render(<<SASS)
.mixed {
  required: foo;
  arg1: non-default-val1;
  arg2: non-default-val2; }
CSS
=a-mixin($required, $arg1: default-val1, $arg2: default-val2)
  required: $required
  arg1: $arg1
  arg2: $arg2
.mixed
  +a-mixin($arg2: non-default-val2, $arg1: non-default-val1, $required: foo)
SASS
  end

  def test_function_output_with_comma
    assert_equal <<CSS, render(<<SASS)
foo {
  a: b(c), d(e); }
CSS
foo
  a: b(c), d(e)
SASS
  end

  def test_interpolation_with_comma
    assert_equal <<CSS, render(<<SASS)
foo {
  a: foo, bar; }
CSS
$foo: foo
foo
  a: \#{$foo}, bar
SASS
  end

  def test_string_interpolation_with_comma
    assert_equal <<CSS, render(<<SASS)
foo {
  a: "bip foo bap", bar; }
CSS
$foo: foo
foo
  a: "bip \#{$foo} bap", bar
SASS
  end

  def test_unknown_directive
    assert_equal <<CSS, render(<<SASS)
@baz {
  c: d; }
CSS
@baz
  c: d
SASS
  end

  def test_comment_interpolation_warning
    assert_warning(<<END) {render("/* \#{foo}")}
WARNING:
On line 1 of 'test_comment_interpolation_warning_inline.sass'
Comments will evaluate the contents of interpolations (\#{ ... }) in Sass 3.2.
Please escape the interpolation by adding a backslash before the `#`.
END
  end

  def test_loud_silent_comment_warning
    assert_warning(<<END) {render("//! \#{foo}")}
WARNING:
On line 1 of 'test_loud_silent_comment_warning_inline.sass'
`//` comments will no longer be allowed to use the `!` flag in Sass 3.2.
Please change to `/*` comments.
END
  end

  def test_loud_comment_interpolations_can_be_escaped
    assert_equal <<CSS, render(<<SASS)
/* \#{foo} */
CSS
/* \\\#{foo}
SASS
    assert_equal <<CSS, render(<<SASS)
/* \#{foo} */
CSS
/*! \\\#{foo}
SASS
  end

  # Encodings

  unless Sass::Util.ruby1_8?
    def test_encoding_error
      render("foo\nbar\nb\xFEaz".force_encoding("utf-8"))
      assert(false, "Expected exception")
    rescue Sass::SyntaxError => e
      assert_equal(3, e.sass_line)
      assert_equal('Invalid UTF-8 character "\xFE"', e.message)
    end

    def test_ascii_incompatible_encoding_error
      template = "foo\nbar\nb_z".encode("utf-16le")
      template[9] = "\xFE".force_encoding("utf-16le")
      render(template)
      assert(false, "Expected exception")
    rescue Sass::SyntaxError => e
      assert_equal(3, e.sass_line)
      assert_equal('Invalid UTF-16LE character "\xFE"', e.message)
    end

    def test_same_charset_as_encoding
      assert_renders_encoded(<<CSS, <<SASS)
@charset "UTF-8";
fóó {
  a: b; }
CSS
@charset "utf-8"
fóó
  a: b
SASS
    end

    def test_different_charset_than_encoding
      assert_renders_encoded(<<CSS.force_encoding("IBM866"), <<SASS)
@charset "IBM866";
fóó {
  a: b; }
CSS
@charset "ibm866"
fóó
  a: b
SASS
    end

    def test_different_encoding_than_system
      assert_renders_encoded(<<CSS.encode("IBM866"), <<SASS.encode("IBM866"))
@charset "IBM866";
тАЬ {
  a: b; }
CSS
тАЬ
  a: b
SASS
    end

    def test_multibyte_charset
      assert_renders_encoded(<<CSS.encode("UTF-16LE"), <<SASS.encode("UTF-16LE").force_encoding("UTF-8"))
@charset "UTF-16LE";
fóó {
  a: b; }
CSS
@charset "utf-16le"
fóó
  a: b
SASS
    end

    def test_multibyte_charset_without_endian_specifier
      assert_renders_encoded(<<CSS.encode("UTF-32BE"), <<SASS.encode("UTF-32BE").force_encoding("UTF-8"))
@charset "UTF-32BE";
fóó {
  a: b; }
CSS
@charset "utf-32"
fóó
  a: b
SASS
    end

    def test_utf8_bom
      assert_renders_encoded(<<CSS, <<SASS.force_encoding("BINARY"))
@charset "UTF-8";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
    end

    def test_utf16le_bom
      assert_renders_encoded(<<CSS.encode("UTF-16LE"), <<SASS.encode("UTF-16LE").force_encoding("BINARY"))
@charset "UTF-16LE";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
    end

    def test_utf32be_bom
      assert_renders_encoded(<<CSS.encode("UTF-32BE"), <<SASS.encode("UTF-32BE").force_encoding("BINARY"))
@charset "UTF-32BE";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
    end
  end

  def test_original_filename_set
    importer = MockImporter.new
    importer.add_import("imported", "div{color:red}")

    original_filename = filename_for_test
    engine = Sass::Engine.new('@import "imported"; div{color:blue}',
      :filename => original_filename, :load_paths => [importer], :syntax => :scss)
    engine.render

    assert_equal original_filename, engine.options[:original_filename]
    assert_equal original_filename, importer.engine("imported").options[:original_filename]
  end

  def test_deprecated_PRECISION
    assert_warning(<<END) {assert_equal 1000.0, Sass::Script::Number::PRECISION}
Sass::Script::Number::PRECISION is deprecated and will be removed in a future release. Use Sass::Script::Number.precision_factor instead.
END
  end
  def test_changing_precision
    begin
      Sass::Script::Number.precision = 8
      assert_equal <<CSS, render(<<SASS)
div {
  maximum: 1.00000001;
  too-much: 1.0; }
CSS
div
  maximum : 1.00000001
  too-much: 1.000000001
SASS
    ensure
      Sass::Script::Number.precision = 3
    end
  end

  private

  def assert_hash_has(hash, expected)
    expected.each {|k, v| assert_equal(v, hash[k])}
  end

  def assert_renders_encoded(css, sass)
    result = render(sass)
    assert_equal css.encoding, result.encoding
    assert_equal css, result
  end

  def render(sass, options = {})
    munge_filename options
    Sass::Engine.new(sass, options).render
  end

  def renders_correctly(name, options={})
    sass_file  = load_file(name, "sass")
    css_file   = load_file(name, "css")
    options[:filename] ||= filename(name, "sass")
    options[:syntax] ||= :sass
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
    engine = Sass::Engine.new("", :filename => sassc_path,
      :importer => Sass::Importers::Filesystem.new("."))
    key = engine.send(:sassc_key)
    File.join(engine.options[:cache_location], key)
  end
end
 
