#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/engine'

class ScssRxTest < MiniTest::Test
  include Sass::SCSS::RX

  def test_identifiers
    assert_match IDENT, "foo"
    assert_match IDENT, "\xC3\xBFoo" # Initial char can be nonascii
    assert_match IDENT, "\\123abcoo" # Initial char can be unicode escape
    assert_match IDENT, "\\f oo" # Unicode escapes can be followed by whitespace
    assert_match IDENT, "\\fa\too"
    assert_match IDENT, "\\ff2\roo"
    assert_match IDENT, "\\f13a\foo"
    assert_match IDENT, "\\f13abcoo"
    assert_match IDENT, "\\ oo" # Initial char can be a plain escape as well
    assert_match IDENT, "\\~oo"
    assert_match IDENT, "\\\\oo"
    assert_match IDENT, "\\{oo"
    assert_match IDENT, "\\\xC3\xBFoo"
    assert_match IDENT, "-foo" # Can put a - before anything
    assert_match IDENT, "-\xC3\xBFoo"
    assert_match IDENT, "-\\f oo"
    assert_match IDENT, "_foo" # Can put a _ before anything
    assert_match IDENT, "_\xC3\xBFoo"
    assert_match IDENT, "_\\f oo"
    assert_match IDENT, "--foo" # "Custom" identifier

    assert_match IDENT, "foo-bar"
    assert_match IDENT, "f012-23"
    assert_match IDENT, "foo_-_bar"
    assert_match IDENT, "f012_23"

    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/escapes-003.xht
    assert_match IDENT, "c\\lass"
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/escapes-004.xht
    assert_match IDENT, "c\\00006Cas\\000073"
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/ident-001.xht
    assert_match IDENT, "IdE6n-3t0_6"
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/ident-006.xht
    assert_match IDENT, "\\6000ident"
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/ident-007.xht
    assert_match IDENT, "iden\\6000t\\6000"
    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/ident-013.xht
    assert_match IDENT, "\\-ident"
  end

  def test_underscores_in_identifiers
    assert_match IDENT, "foo_bar"
    assert_match IDENT, "_\xC3\xBFfoo"
    assert_match IDENT, "__foo"
    assert_match IDENT, "_1foo"
    assert_match IDENT, "-_foo"
    assert_match IDENT, "_-foo"
  end

  def test_invalid_identifiers
    assert_no_match IDENT, ""
    assert_no_match IDENT, "1foo"
    assert_no_match IDENT, "-1foo"
    assert_no_match IDENT, "foo bar"
    assert_no_match IDENT, "foo~bar"

    # http://www.w3.org/Style/CSS/Test/CSS2.1/current/xhtml1/escapes-008.xht
    assert_no_match IDENT, "c\\06C  ass"
    assert_no_match IDENT, "back\\67\n round"
  end

  def test_double_quote_strings
    assert_match STRING, '"foo bar"'
    assert_match STRING, '"foo\\\nbar"'
    assert_match STRING, "\"\\\"\""
    assert_match STRING, '"\t !#$%&(-~()*+,-./0123456789~"'
  end

  def test_single_quote_strings
    assert_match STRING, "'foo bar'"
    assert_match STRING, "'foo\\\nbar'"
    assert_match STRING, "'\\''"
    assert_match STRING, "'\t !#\$%&(-~()*+,-./0123456789~'"
  end

  def test_invalid_strings
    assert_no_match STRING, "\"foo\nbar\""
    assert_no_match STRING, "\"foo\"bar\""
    assert_no_match STRING, "'foo\nbar'"
    assert_no_match STRING, "'foo'bar'"
  end

  def test_uri
    assert_match URI, 'url("foo bar)")'
    assert_match URI, "url('foo bar)')"
    assert_match URI, 'url( "foo bar)" )'
    assert_match URI, "url(#\\%&**+,-./0123456789~)"
  end

  def test_invalid_uri
    assert_no_match URI, 'url(foo)bar)'
  end

  def test_unicode_range
    assert_match UNICODERANGE, 'U+00-Ff'
    assert_match UNICODERANGE, 'u+980-9FF'
    assert_match UNICODERANGE, 'U+9aF??'
    assert_match UNICODERANGE, 'U+??'
  end

  def test_escape_empty_ident
    assert_equal "", Sass::SCSS::RX.escape_ident("")
  end

  def test_escape_just_prefix_ident
    assert_equal "\\-", Sass::SCSS::RX.escape_ident("-")
    assert_equal "\\_", Sass::SCSS::RX.escape_ident("_")
  end

  def test_escape_plain_ident
    assert_equal "foo", Sass::SCSS::RX.escape_ident("foo")
    assert_equal "foo-1bar", Sass::SCSS::RX.escape_ident("foo-1bar")
    assert_equal "-foo-bar", Sass::SCSS::RX.escape_ident("-foo-bar")
    assert_equal "f2oo_bar", Sass::SCSS::RX.escape_ident("f2oo_bar")
    assert_equal "_foo_bar", Sass::SCSS::RX.escape_ident("_foo_bar")
  end

  def test_escape_initial_funky_ident
    assert_equal "\\000035foo", Sass::SCSS::RX.escape_ident("5foo")
    assert_equal "-\\000035foo", Sass::SCSS::RX.escape_ident("-5foo")
    assert_equal "_\\000035foo", Sass::SCSS::RX.escape_ident("_5foo")

    assert_equal "\\&foo", Sass::SCSS::RX.escape_ident("&foo")
    assert_equal "-\\&foo", Sass::SCSS::RX.escape_ident("-&foo")

    assert_equal "-\\ foo", Sass::SCSS::RX.escape_ident("- foo")
  end

  def test_escape_mid_funky_ident
    assert_equal "foo\\&bar", Sass::SCSS::RX.escape_ident("foo&bar")
    assert_equal "foo\\ \\ bar", Sass::SCSS::RX.escape_ident("foo  bar")
    assert_equal "foo\\00007fbar", Sass::SCSS::RX.escape_ident("foo\177bar")
  end

  def test_no_static_hyphenated_units
    assert_no_match STATIC_VALUE, "20px-20px"
  end

  private

  def assert_match(rx, str)
    refute_nil(match = rx.match(str))
    assert_equal str.size, match[0].size
  end

  def assert_no_match(rx, str)
    match = rx.match(str)
    refute_equal str.size, match && match[0].size
  end

end
