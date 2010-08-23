#!/usr/bin/env ruby
require 'test/unit'
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/css'

class CSS2SassTest < Test::Unit::TestCase
  def test_basic
    css = <<CSS
h1 {
  color: red;
}
CSS
    assert_equal(<<SASS, css2sass(css))
h1
  color: red
SASS
    assert_equal(<<SASS, css2sass(css, :old => true))
h1
  :color red
SASS
  end

  def test_nesting
    assert_equal(<<SASS, css2sass(<<CSS))
li
  display: none
  a
    text-decoration: none
    span
      color: yellow
    &:hover
      text-decoration: underline
SASS
li {
  display: none;
}

li a {
  text-decoration: none;
}

li a span {
  color: yellow;
}

li a:hover {
  text-decoration: underline;
}
CSS
  end

  def test_no_nesting_around_rules
    assert_equal(<<SASS, css2sass(<<CSS))
div .warning
  color: #d21a19

span .debug
  cursor: crosshair

div .debug
  cursor: default
SASS
div .warning {
  color: #d21a19; }
span .debug { 
  cursor: crosshair;}
div .debug {
  cursor: default; }
CSS
  end

  def test_comments_multiline
    css = <<CSS
/* comment */
elephant.rawr {
  rampages: excessively;
}

/* actual multiline
  comment */
span.turkey {
  isdinner: true;
}

.turducken {
  /* Sounds funny
     doesn't it */
  chimera: not_really;
}

#overhere {
  bored: sorta; /* it's for a good
  cause */
  better_than: thread_pools;
}

#one_more {
  finally: srsly;
} /* just a line here */
CSS
    sass = <<SASS
/* comment

elephant.rawr
  rampages: excessively

/* actual multiline
 *comment

span.turkey
  isdinner: true

.turducken
  /* Sounds funny
   * doesn't it
  chimera: not_really

#overhere
  bored: sorta
  /*                  it's for a good
   * cause
  better_than: thread_pools

#one_more
  finally: srsly

/* just a line here
SASS
    assert_equal(sass, css2sass(css))
  end

  def test_fold_commas
    assert_equal(<<SASS, css2sass(<<CSS))
li
  .one, .two
    color: red
SASS
li .one {
  color: red;
}
li .two {
  color: red;
}
CSS

    assert_equal(<<SASS, css2sass(<<CSS))
.one
  color: green

.two
  color: green
  color: red

.three
  color: red
SASS
.one, .two {
  color: green;
}

.two, .three {
  color: red;
}
CSS
  end

  def test_bad_formatting
    assert_equal(<<SASS, css2sass(<<CSS))
hello
  parent: true
  there
    parent: false
  who
    hoo: false
  why
    y: true
  when
    wen: nao

down_here
  yeah: true
SASS
hello {
  parent: true;
}

hello  there {
  parent: false;
}
hello who  {
  hoo: false;
}
hello why {
   y: true;
}
hello when {
  wen:  nao;
}



down_here { yeah: true; }
CSS
  end

  def test_comments_in_selectors
    assert_equal(<<SASS, css2sass(<<CSS))
.js
  #location-navigation-form .form-submit, #business-listing-form .form-submit, #detailTabs ul, #detailsEnhanced #addTags, #locationSearchList, #moreHoods
    display: none

#navListLeft
  display: none
SASS
.js #location-navigation-form .form-submit,
.js #business-listing-form .form-submit,
.js #detailTabs ul,
/* .js #addReview, */
/* .js #addTags, */
.js #detailsEnhanced #addTags,
.js #locationSearchList,
.js #moreHoods,
#navListLeft
  { display: none; }
CSS
  end

  def test_pseudo_classes_are_escaped
    assert_equal(<<SASS, css2sass(<<CSS))
\\:focus
  a: b
  \\:foo
    bar: baz
SASS
:focus {a: b;}
:focus :foo {bar: baz;}
CSS
  end

  # Regressions

  def test_nesting_within_media
    assert_equal(<<SASS, css2sass(<<CSS))
@media all
  .next .vevent
    padding-left: 0
    padding-right: 0
SASS
@media all {
  .next .vevent {
    padding-left: 0;
    padding-right: 0; } }
CSS
  end

  def test_multiline_selector_within_media_and_with_child_selector
    assert_equal(<<SASS, css2sass(<<CSS))
@media all
  foo bar, baz
    padding-left: 0
    padding-right: 0
SASS
@media all {
  foo bar,
  baz {
    padding-left: 0;
    padding-right: 0; } }
CSS
  end

  # Error reporting

  def test_error_reporting
    css2sass("foo")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(1, err.sass_line)
    assert_equal('Invalid CSS after "foo": expected "{", was ""', err.message)
  end

  def test_error_reporting_in_line
    css2sass("foo\nbar }\nbaz")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(2, err.sass_line)
    assert_equal('Invalid CSS after "bar ": expected "{", was "}"', err.message)
  end

  def test_error_truncate_after
    css2sass("#{"a" * 16}foo")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(1, err.sass_line)
    assert_equal('Invalid CSS after "...aaaaaaaaaaaafoo": expected "{", was ""', err.message)
  end

  def test_error_truncate_was
    css2sass("foo }foo#{"a" * 15}")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(1, err.sass_line)
    assert_equal('Invalid CSS after "foo ": expected "{", was "}fooaaaaaaaaaaa..."', err.message)
  end

  def test_error_doesnt_truncate_after_when_elipsis_would_add_length
    css2sass("#{"a" * 15}foo")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(1, err.sass_line)
    assert_equal('Invalid CSS after "aaaaaaaaaaaaaaafoo": expected "{", was ""', err.message)
  end

  def test_error_doesnt_truncate_was_when_elipsis_would_add_length
    css2sass("foo }foo#{"a" * 14}")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(1, err.sass_line)
    assert_equal('Invalid CSS after "foo ": expected "{", was "}fooaaaaaaaaaaaaaa"', err.message)
  end

  def test_error_gets_rid_of_trailing_newline_for_after
    css2sass("foo  \n  ")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(2, err.sass_line)
    assert_equal('Invalid CSS after "foo": expected "{", was ""', err.message)
  end

  def test_error_gets_rid_of_trailing_newline_for_was
    css2sass("foo \n  }foo")
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => err
    assert_equal(2, err.sass_line)
    assert_equal('Invalid CSS after "foo": expected "{", was "}foo"', err.message)
  end

  # Encodings

  unless Sass::Util.ruby1_8?
    def test_encoding_error
      css2sass("foo\nbar\nb\xFEaz".force_encoding("utf-8"))
      assert(false, "Expected exception")
    rescue Sass::SyntaxError => e
      assert_equal(3, e.sass_line)
      assert_equal('Invalid UTF-8 character "\xFE"', e.message)
    end

    def test_ascii_incompatible_encoding_error
      template = "foo\nbar\nb_z".encode("utf-16le")
      template[9] = "\xFE".force_encoding("utf-16le")
      css2sass(template)
      assert(false, "Expected exception")
    rescue Sass::SyntaxError => e
      assert_equal(3, e.sass_line)
      assert_equal('Invalid UTF-16LE character "\xFE"', e.message)
    end
  end

  private

  def css2sass(string, opts={})
    Sass::CSS.new(string, opts).render
  end
end
