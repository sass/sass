#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'

class SourcemapTest < Test::Unit::TestCase
  def test_simple_scss_mapping
    assert_parses_with_sourcemap <<SCSS, <<CSS, <<JSON
a {
  foo: bar;
/* SOME COMMENT */
  font-size: 12px;
}
SCSS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";EACE,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["test_simple_scss_mapping_inline.scss"],
"file": "test.css"
}
JSON
  end

  def test_mapping_with_directory
    options = {:filename => "scss/style.scss", :output => "css/style.css"}
    assert_parses_with_sourcemap <<SCSS, <<CSS, <<JSON, options
a {
  foo: bar;
/* SOME COMMENT */
  font-size: 12px;
}
SCSS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*@ sourceMappingURL=style.css.map */
CSS
{
"version": "3",
"mappings": ";EACE,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["..\\/scss\\/style.scss"],
"file": "style.css"
}
JSON
  end

  unless Sass::Util.ruby1_8?
    def test_simple_charset_scss_mapping
      assert_parses_with_sourcemap <<SCSS, <<CSS, <<JSON
a {
  fóó: bár;
}
SCSS
@charset "UTF-8";
a {
  fóó: bár; }

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";;EACE,GAAG,EAAE,GAAG",
"sources": ["test_simple_charset_scss_mapping_inline.scss"],
"file": "test.css"
}
JSON
    end

    def test_different_charset_than_encoding
      assert_parses_with_sourcemap(<<CSS.force_encoding("IBM866"), <<SASS.force_encoding("IBM866"), <<JSON)
@charset "IBM866";
f\x86\x86 {
  \x86: b;
}
CSS
@charset "IBM866";
f\x86\x86 {
  \x86: b; }

/*@ sourceMappingURL=test.css.map */
SASS
{
"version": "3",
"mappings": ";;EAEE,CAAC,EAAE,CAAC",
"sources": ["test_different_charset_than_encoding_inline.scss"],
"file": "test.css"
}
JSON
    end
  end

  def assert_parses_with_sourcemap(scss, css, sourcemap_json, options={})
    rendered, sourcemap = render_with_sourcemap(scss, options)
    assert_equal css.rstrip, rendered.rstrip
    assert_equal sourcemap_json.rstrip, sourcemap.to_json(options[:output] || "test.css")
  end

  def render_with_sourcemap(scss, options={})
    options[:syntax] ||= :scss
    munge_filename options
    engine = Sass::Engine.new(scss, options)
    engine.options[:cache] = false
    sourcemap_path = Sass::Util.sourcemap_name(options[:output] || "test.css")
    engine.render_with_sourcemap File.basename(sourcemap_path)
  end
end
