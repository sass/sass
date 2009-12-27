#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/engine'

class ScssRxTest < Test::Unit::TestCase
  include Sass::SCSS::RX

  def test_identifiers
    assert_match IDENT, "foo"
    assert_match IDENT, "\377oo" # Initial char can be nonascii
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
    assert_match IDENT, "\\\377oo"
    assert_match IDENT, "-foo" # Can put a - before anything
    assert_match IDENT, "-\377oo"
    assert_match IDENT, "-\\f oo"

    assert_match IDENT, "foo-bar"
    assert_match IDENT, "f012-23"
  end

  def test_underscores_in_identifiers
    assert_match IDENT, "foo_bar"
    assert_match IDENT, "_\377foo"
  end

  def test_invalid_identifiers
    assert_no_match IDENT, "1foo"
    assert_no_match IDENT, "-1foo"
    assert_no_match IDENT, "--foo"
    assert_no_match IDENT, "_1foo"
    assert_no_match IDENT, "__foo"
    assert_no_match IDENT, "-_foo"
    assert_no_match IDENT, "_-foo"
    assert_no_match IDENT, "foo bar"
    assert_no_match IDENT, "foo~bar"
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
    assert_match URI, "url(!#\$%&**+,-./0123456789~)"
  end

  def test_invalid_uri
    assert_no_match URI, 'url(foo)bar)'
  end

  def test_unicode_range
    assert_match UNICODERANGE, 'U+00-FF'
    assert_match UNICODERANGE, 'U+980-9FF'
    assert_match UNICODERANGE, 'U+9??'
    assert_match UNICODERANGE, 'U+??'
  end

  private

  def assert_match(rx, str)
    super /^#{rx}$/, str
  end

  def assert_no_match(rx, str)
    super /^#{rx}$/, str
  end

end
