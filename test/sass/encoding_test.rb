#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/util/test'

class EncodingTest < MiniTest::Test
  include Sass::Util::Test

  def test_encoding_error
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    render("foo\nbar\nb\xFEaz".force_encoding("utf-8"))
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal(3, e.sass_line)
    assert_equal('Invalid UTF-8 character "\xFE"', e.message)
  end

  def test_ascii_incompatible_encoding_error
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    template = "foo\nbar\nb_z".encode("utf-16le")
    template[9] = "\xFE".force_encoding("utf-16le")
    render(template)
    assert(false, "Expected exception")
  rescue Sass::SyntaxError => e
    assert_equal(3, e.sass_line)
    assert_equal('Invalid UTF-16LE character "\xFE"', e.message)
  end

  def test_prefers_charset_to_ruby_encoding
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_renders_encoded(<<CSS, <<SASS.encode("IBM866").force_encoding("UTF-8"))
@charset "UTF-8";
fЖЖ {
  a: b; }
CSS
@charset "ibm866"
fЖЖ
  a: b
SASS
  end

  def test_uses_ruby_encoding_without_charset
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_renders_encoded(<<CSS, <<SASS.encode("IBM866"))
@charset "UTF-8";
тАЬ {
  a: b; }
CSS
тАЬ
  a: b
SASS
  end

  def test_multibyte_charset_without_bom_declared_as_binary
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    engine = Sass::Engine.new(<<SASS.encode("UTF-16LE").force_encoding("BINARY"))
@charset "utf-16le"
fóó
  a: b
SASS
    # Since multibyte encodings' @charset declarations aren't
    # ASCII-compatible, we have to interpret the files as UTF-8 which will
    # inevitably fail.
    assert_raise_message(Sass::SyntaxError, "Invalid UTF-8 character \"\\xF3\"") {engine.render}
  end

  def test_multibyte_charset_without_bom_declared_as_utf_8
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    engine = Sass::Engine.new(<<SASS.encode("UTF-16LE").force_encoding("UTF-8"))
@charset "utf-16le"
fóó
  a: b
SASS
    # Since multibyte encodings' @charset declarations aren't
    # ASCII-compatible, we have to interpret the files as UTF-8 which will
    # inevitably fail.
    assert_raise_message(Sass::SyntaxError, "Invalid UTF-8 character \"\\xF3\"") {engine.render}
  end

  def test_utf_16le_with_bom
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_renders_encoded(<<CSS, <<SASS.encode("UTF-16LE").force_encoding("BINARY"))
@charset "UTF-8";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
  end

  def test_utf_16be_with_bom
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_renders_encoded(<<CSS, <<SASS.encode("UTF-16BE").force_encoding("BINARY"))
@charset "UTF-8";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
  end

  def test_utf_8_with_bom
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_renders_encoded(<<CSS, <<SASS.force_encoding("BINARY"))
@charset "UTF-8";
fóó {
  a: b; }
CSS
\uFEFFfóó
  a: b
SASS
  end

  def test_charset_with_multibyte_encoding
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    engine = Sass::Engine.new(<<SASS)
@charset "utf-32be"
fóó
  a: b
SASS
    # The charset declaration is just false here, so we should get an
    # encoding error.
    assert_raise_message(Sass::SyntaxError, "Invalid UTF-32BE character \"\\xC3\"") {engine.render}
  end

  def test_charset_with_special_case_encoding
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    # For some reason, a file with an ASCII-compatible UTF-16 charset
    # declaration is specced to be parsed as UTF-8.
    assert_renders_encoded(<<CSS, <<SASS.force_encoding("BINARY"))
@charset "UTF-8";
fóó {
  a: b; }
CSS
@charset "utf-16"
fóó
  a: b
SASS
  end

  def test_compressed_output_uses_bom
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_equal("\uFEFFfóó{a:b}\n", render(<<SASS, :style => :compressed))
fóó
  a: b
SASS
  end

  def test_newline_normalization
    assert_equal("/* foo\nbar\nbaz\nbang\nqux */\n",
      render("/* foo\nbar\r\nbaz\fbang\rqux */", :syntax => :scss))
  end

  def test_null_normalization
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?
    
    assert_equal(<<CSS, render("/* foo\x00bar\x00baz */", :syntax => :scss))
#{"@charset \"UTF-8\";\n" unless Sass::Util.ruby1_8?
}/* foo�bar�baz */
CSS
  end

  # Regression

  def test_multibyte_prop_name
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_equal(<<CSS, render(<<SASS))
@charset "UTF-8";
#bar {
  cölor: blue; }
CSS
#bar
  cölor: blue
SASS
  end

  def test_multibyte_and_interpolation
    return skip "Can't be run on Ruby 1.8." if Sass::Util.ruby1_8?

    assert_equal(<<CSS, render(<<SCSS, :syntax => :scss))
#bar {
  background: a 0%; }
CSS
#bar {
  // 
  background: \#{a} 0%;
}
SCSS
  end

  private

  def assert_renders_encoded(css, sass)
    result = render(sass)
    assert_equal css.encoding, result.encoding
    assert_equal css, result
  end

  def render(sass, options = {})
    munge_filename options
    Sass::Engine.new(sass, options).render
  end
end
