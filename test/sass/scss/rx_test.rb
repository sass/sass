#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/engine'

class ScssRxTest < Test::Unit::TestCase
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
  end

  def test_invalid_identifiers
    assert_no_match IDENT, ""
    assert_no_match IDENT, "1foo"
    assert_no_match IDENT, "-1foo"
    assert_no_match IDENT, "--foo"
    assert_no_match IDENT, "_1foo"
    assert_no_match IDENT, "__foo"
    assert_no_match IDENT, "-_foo"
    assert_no_match IDENT, "_-foo"
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
    assert_match URI, "url(!#\$%&**+,-./0123456789~)"
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

  private

  def assert_match(rx, str)
    assert_not_nil(match = rx.match(str))
    assert_equal str.size, match[0].size
  end

  def assert_no_match(rx, str)
    match = rx.match(str)
    assert_not_equal str.size, match && match[0].size
  end

end
