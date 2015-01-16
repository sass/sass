#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'

class SourcemapTest < MiniTest::Test
  def test_to_json_requires_args
    _, sourcemap = render_with_sourcemap('')
    assert_raises(ArgumentError) {sourcemap.to_json({})}
    assert_raises(ArgumentError) {sourcemap.to_json({:css_path => 'foo'})}
    assert_raises(ArgumentError) {sourcemap.to_json({:sourcemap_path => 'foo'})}
  end

  def test_simple_mapping_scss
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

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": "AAAA,CAAE;EACA,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["test_simple_mapping_scss_inline.scss"],
"names": [],
"file": "test.css"
}
JSON
  end

  def test_simple_mapping_sass
    assert_parses_with_sourcemap <<SASS, <<CSS, <<JSON, :syntax => :sass
a
  foo: bar
  /* SOME COMMENT */
  :font-size 12px
SASS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": "AAAA,CAAC;EACC,GAAG,EAAE,GAAG;;EAEP,SAAS,EAAC,IAAI",
"sources": ["test_simple_mapping_sass_inline.sass"],
"names": [],
"file": "test.css"
}
JSON
  end

  def test_simple_mapping_with_file_uris
    uri = Sass::Util.file_uri_from_path(Sass::Util.absolute_path(filename_for_test(:scss)))
    assert_parses_with_sourcemap <<SCSS, <<CSS, <<JSON, :sourcemap => :file
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

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": "AAAA,CAAE;EACA,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["#{uri}"],
"names": [],
"file": "test.css"
}
JSON
  end

  def test_mapping_with_directory_scss
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

/*# sourceMappingURL=style.css.map */
CSS
{
"version": 3,
"mappings": "AAAA,CAAE;EACA,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["../scss/style.scss"],
"names": [],
"file": "style.css"
}
JSON
  end

  def test_mapping_with_directory_sass
    options = {:filename => "sass/style.sass", :output => "css/style.css", :syntax => :sass}
    assert_parses_with_sourcemap <<SASS, <<CSS, <<JSON, options
a
  foo: bar
  /* SOME COMMENT */
  :font-size 12px
SASS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*# sourceMappingURL=style.css.map */
CSS
{
"version": 3,
"mappings": "AAAA,CAAC;EACC,GAAG,EAAE,GAAG;;EAEP,SAAS,EAAC,IAAI",
"sources": ["../sass/style.sass"],
"names": [],
"file": "style.css"
}
JSON
  end

  unless Sass::Util.ruby1_8?
    def test_simple_charset_mapping_scss
      assert_parses_with_sourcemap <<SCSS, <<CSS, <<JSON
a {
  fóó: bár;
}
SCSS
@charset "UTF-8";
a {
  fóó: bár; }

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": ";AAAA,CAAE;EACA,GAAG,EAAE,GAAG",
"sources": ["test_simple_charset_mapping_scss_inline.scss"],
"names": [],
"file": "test.css"
}
JSON
    end

    def test_simple_charset_mapping_sass
      assert_parses_with_sourcemap <<SASS, <<CSS, <<JSON, :syntax => :sass
a
  fóó: bár
SASS
@charset "UTF-8";
a {
  fóó: bár; }

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": ";AAAA,CAAC;EACC,GAAG,EAAE,GAAG",
"sources": ["test_simple_charset_mapping_sass_inline.sass"],
"names": [],
"file": "test.css"
}
JSON
    end

    def test_different_charset_than_encoding_scss
      assert_parses_with_sourcemap(<<SCSS.force_encoding("IBM866"), <<CSS, <<JSON)
@charset "IBM866";
f\x86\x86 {
  \x86: b;
}
SCSS
@charset "UTF-8";
fЖЖ {
  Ж: b; }

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": ";AACA,GAAI;EACF,CAAC,EAAE,CAAC",
"sources": ["test_different_charset_than_encoding_scss_inline.scss"],
"names": [],
"file": "test.css"
}
JSON
    end

    def test_different_charset_than_encoding_sass
      assert_parses_with_sourcemap(<<SASS.force_encoding("IBM866"), <<CSS, <<JSON, :syntax => :sass)
@charset "IBM866"
f\x86\x86
  \x86: b
SASS
@charset "UTF-8";
fЖЖ {
  Ж: b; }

/*# sourceMappingURL=test.css.map */
CSS
{
"version": 3,
"mappings": ";AACA,GAAG;EACD,CAAC,EAAE,CAAC",
"sources": ["test_different_charset_than_encoding_sass_inline.sass"],
"names": [],
"file": "test.css"
}
JSON
    end
  end

  def test_import_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@import {{1}}url(foo){{/1}},{{2}}url(moo)   {{/2}},       {{3}}url(bar) {{/3}};
@import {{4}}url(baz) screen print{{/4}};
SCSS
{{1}}@import url(foo){{/1}};
{{2}}@import url(moo){{/2}};
{{3}}@import url(bar){{/3}};
{{4}}@import url(baz) screen print{{/4}};

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_import_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@import {{1}}foo.css{{/1}},{{2}}moo.css{{/2}},      {{3}}bar.css{{/3}}
@import {{4}}url(baz.css){{/4}}
@import {{5}}url(qux.css) screen print{{/5}}
SASS
{{1}}@import url(foo.css){{/1}};
{{2}}@import url(moo.css){{/2}};
{{3}}@import url(bar.css){{/3}};
{{4}}@import url(baz.css){{/4}};
{{5}}@import url(qux.css) screen print{{/5}};

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_media_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
{{1}}@media screen, tv  {{/1}}{
  {{2}}body {{/2}}{
    {{3}}max-width{{/3}}: {{4}}1070px{{/4}};
  }
}
SCSS
{{1}}@media screen, tv{{/1}} {
  {{2}}body{{/2}} {
    {{3}}max-width{{/3}}: {{4}}1070px{{/4}}; } }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_media_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
{{1}}@media screen, tv{{/1}}
  {{2}}body{{/2}}
    {{3}}max-width{{/3}}: {{4}}1070px{{/4}}
SASS
{{1}}@media screen, tv{{/1}} {
  {{2}}body{{/2}} {
    {{3}}max-width{{/3}}: {{4}}1070px{{/4}}; } }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_interpolation_and_vars_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$te: "te";
$teal: {{5}}teal{{/5}};
{{1}}p {{/1}}{
  {{2}}con#{$te}nt{{/2}}: {{3}}"I a#{$te} #{5 + 10} pies!"{{/3}};
  {{4}}color{{/4}}: $teal;
}

$name: foo;
$attr: border;
{{6}}p.#{$name} {{/6}}{
  {{7}}#{$attr}-color{{/7}}: {{8}}blue{{/8}};
  $font-size: 12px;
  $line-height: 30px;
  {{9}}font{{/9}}: {{10}}#{$font-size}/#{$line-height}{{/10}};
}
SCSS
{{1}}p{{/1}} {
  {{2}}content{{/2}}: {{3}}"I ate 15 pies!"{{/3}};
  {{4}}color{{/4}}: {{5}}teal{{/5}}; }

{{6}}p.foo{{/6}} {
  {{7}}border-color{{/7}}: {{8}}blue{{/8}};
  {{9}}font{{/9}}: {{10}}12px/30px{{/10}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_interpolation_and_vars_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$te: "te"
$teal: {{5}}teal{{/5}}
{{1}}p{{/1}}
  {{2}}con#{$te}nt{{/2}}: {{3}}"I a#{$te} #{5 + 10} pies!"{{/3}}
  {{4}}color{{/4}}: $teal

$name: foo
$attr: border
{{6}}p.#{$name}{{/6}}
  {{7}}#{$attr}-color{{/7}}: {{8}}blue{{/8}}
  $font-size: 12px
  $line-height: 30px
  {{9}}font{{/9}}: {{10}}#{$font-size}/#{$line-height}{{/10}}
SASS
{{1}}p{{/1}} {
  {{2}}content{{/2}}: {{3}}"I ate 15 pies!"{{/3}};
  {{4}}color{{/4}}: {{5}}teal{{/5}}; }

{{6}}p.foo{{/6}} {
  {{7}}border-color{{/7}}: {{8}}blue{{/8}};
  {{9}}font{{/9}}: {{10}}12px/30px{{/10}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_selectors_properties_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$width: 2px;
$translucent-red: rgba(255, 0, 0, 0.5);
{{1}}a {{/1}}{
  {{8}}.special {{/8}}{
    {{9}}color{{/9}}: {{10}}red{{/10}};
    {{11}}&:hover {{/11}}{
      {{12}}foo{{/12}}: {{13}}bar{{/13}};
      {{14}}cursor{{/14}}: {{15}}e + -resize{{/15}};
      {{16}}color{{/16}}: {{17}}opacify($translucent-red, 0.3){{/17}};
    }
    {{18}}&:after {{/18}}{
      {{19}}content{{/19}}: {{20}}"I ate #{5 + 10} pies #{$width} thick!"{{/20}};
    }
  }
  {{21}}&:active {{/21}}{
    {{22}}color{{/22}}: {{23}}#010203 + #040506{{/23}};
    {{24}}border{{/24}}: {{25}}$width solid black{{/25}};
  }
/* SOME COMMENT */
  {{2}}font{{/2}}: {{3}}2px/3px {{/3}}{
    {{4}}family{{/4}}: {{5}}fantasy{{/5}};
    {{6}}size{{/6}}: {{7}}1em + (2em * 3){{/7}};
  }
}
SCSS
{{1}}a{{/1}} {
  /* SOME COMMENT */
  {{2}}font{{/2}}: {{3}}2px/3px{{/3}};
    {{4}}font-family{{/4}}: {{5}}fantasy{{/5}};
    {{6}}font-size{{/6}}: {{7}}7em{{/7}}; }
  {{8}}a .special{{/8}} {
    {{9}}color{{/9}}: {{10}}red{{/10}}; }
    {{11}}a .special:hover{{/11}} {
      {{12}}foo{{/12}}: {{13}}bar{{/13}};
      {{14}}cursor{{/14}}: {{15}}e-resize{{/15}};
      {{16}}color{{/16}}: {{17}}rgba(255, 0, 0, 0.8){{/17}}; }
    {{18}}a .special:after{{/18}} {
      {{19}}content{{/19}}: {{20}}"I ate 15 pies 2px thick!"{{/20}}; }
  {{21}}a:active{{/21}} {
    {{22}}color{{/22}}: {{23}}#050709{{/23}};
    {{24}}border{{/24}}: {{25}}2px solid black{{/25}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_selectors_properties_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$width: 2px
$translucent-red: rgba(255, 0, 0, 0.5)
{{1}}a{{/1}}
  {{8}}.special{{/8}}
    {{9}}color{{/9}}: {{10}}red{{/10}}
    {{11}}&:hover{{/11}}
      {{12}}foo{{/12}}: {{13}}bar{{/13}}
      {{14}}cursor{{/14}}: {{15}}e + -resize{{/15}}
      {{16}}color{{/16}}: {{17}}opacify($translucent-red, 0.3){{/17}}
    {{18}}&:after{{/18}}
      {{19}}content{{/19}}: {{20}}"I ate #{5 + 10} pies #{$width} thick!"{{/20}}
  {{21}}&:active{{/21}}
    {{22}}color{{/22}}: {{23}}#010203 + #040506{{/23}}
    {{24}}border{{/24}}: {{25}}$width solid black{{/25}}

  /* SOME COMMENT */
  {{2}}font{{/2}}: {{3}}2px/3px{{/3}}
    {{4}}family{{/4}}: {{5}}fantasy{{/5}}
    {{6}}size{{/6}}: {{7}}1em + (2em * 3){{/7}}
SASS
{{1}}a{{/1}} {
  /* SOME COMMENT */
  {{2}}font{{/2}}: {{3}}2px/3px{{/3}};
    {{4}}font-family{{/4}}: {{5}}fantasy{{/5}};
    {{6}}font-size{{/6}}: {{7}}7em{{/7}}; }
  {{8}}a .special{{/8}} {
    {{9}}color{{/9}}: {{10}}red{{/10}}; }
    {{11}}a .special:hover{{/11}} {
      {{12}}foo{{/12}}: {{13}}bar{{/13}};
      {{14}}cursor{{/14}}: {{15}}e-resize{{/15}};
      {{16}}color{{/16}}: {{17}}rgba(255, 0, 0, 0.8){{/17}}; }
    {{18}}a .special:after{{/18}} {
      {{19}}content{{/19}}: {{20}}"I ate 15 pies 2px thick!"{{/20}}; }
  {{21}}a:active{{/21}} {
    {{22}}color{{/22}}: {{23}}#050709{{/23}};
    {{24}}border{{/24}}: {{25}}2px solid black{{/25}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_extend_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
{{1}}.error {{/1}}{
  {{2}}border{{/2}}: {{3}}1px #ff00aa{{/3}};
  {{4}}background-color{{/4}}: {{5}}#fdd{{/5}};
}
{{6}}.seriousError {{/6}}{
  @extend .error;
  {{7}}border-width{{/7}}: {{8}}3px{{/8}};
}
SCSS
{{1}}.error, .seriousError{{/1}} {
  {{2}}border{{/2}}: {{3}}1px #ff00aa{{/3}};
  {{4}}background-color{{/4}}: {{5}}#fdd{{/5}}; }

{{6}}.seriousError{{/6}} {
  {{7}}border-width{{/7}}: {{8}}3px{{/8}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_extend_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
{{1}}.error{{/1}}
  {{2}}border{{/2}}: {{3}}1px #f00{{/3}}
  {{4}}background-color{{/4}}: {{5}}#fdd{{/5}}

{{6}}.seriousError{{/6}}
  @extend .error
  {{7}}border-width{{/7}}: {{8}}3px{{/8}}
SASS
{{1}}.error, .seriousError{{/1}} {
  {{2}}border{{/2}}: {{3}}1px #f00{{/3}};
  {{4}}background-color{{/4}}: {{5}}#fdd{{/5}}; }

{{6}}.seriousError{{/6}} {
  {{7}}border-width{{/7}}: {{8}}3px{{/8}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_for_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@for $i from 1 through 3 {
  {{1}}{{4}}{{7}}.item-#{$i} {{/1}}{{/4}}{{/7}}{ {{2}}{{5}}{{8}}width{{/2}}{{/5}}{{/8}}: {{3}}{{6}}{{9}}2em * $i{{/3}}{{/6}}{{/9}}; }
}
SCSS
{{1}}.item-1{{/1}} {
  {{2}}width{{/2}}: {{3}}2em{{/3}}; }

{{4}}.item-2{{/4}} {
  {{5}}width{{/5}}: {{6}}4em{{/6}}; }

{{7}}.item-3{{/7}} {
  {{8}}width{{/8}}: {{9}}6em{{/9}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_for_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@for $i from 1 through 3
  {{1}}{{4}}{{7}}.item-#{$i}{{/1}}{{/4}}{{/7}}
    {{2}}{{5}}{{8}}width{{/2}}{{/5}}{{/8}}: {{3}}{{6}}{{9}}2em * $i{{/3}}{{/6}}{{/9}}
SASS
{{1}}.item-1{{/1}} {
  {{2}}width{{/2}}: {{3}}2em{{/3}}; }

{{4}}.item-2{{/4}} {
  {{5}}width{{/5}}: {{6}}4em{{/6}}; }

{{7}}.item-3{{/7}} {
  {{8}}width{{/8}}: {{9}}6em{{/9}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_while_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$i: 6;
@while $i > 0 {
  {{1}}{{4}}{{7}}.item-#{$i} {{/1}}{{/4}}{{/7}}{ {{2}}{{5}}{{8}}width{{/2}}{{/5}}{{/8}}: {{3}}{{6}}{{9}}2em * $i{{/3}}{{/6}}{{/9}}; }
  $i: $i - 2 !global;
}
SCSS
{{1}}.item-6{{/1}} {
  {{2}}width{{/2}}: {{3}}12em{{/3}}; }

{{4}}.item-4{{/4}} {
  {{5}}width{{/5}}: {{6}}8em{{/6}}; }

{{7}}.item-2{{/7}} {
  {{8}}width{{/8}}: {{9}}4em{{/9}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_while_sourcemap_sass
  assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$i: 6
@while $i > 0
  {{1}}{{4}}{{7}}.item-#{$i}{{/1}}{{/4}}{{/7}}
    {{2}}{{5}}{{8}}width{{/2}}{{/5}}{{/8}}: {{3}}{{6}}{{9}}2em * $i{{/3}}{{/6}}{{/9}}
    $i: $i - 2 !global
SASS
{{1}}.item-6{{/1}} {
  {{2}}width{{/2}}: {{3}}12em{{/3}}; }

{{4}}.item-4{{/4}} {
  {{5}}width{{/5}}: {{6}}8em{{/6}}; }

{{7}}.item-2{{/7}} {
  {{8}}width{{/8}}: {{9}}4em{{/9}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_each_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@each $animal in puma, sea-slug, egret, salamander {
  {{1}}{{4}}{{7}}{{10}}.#{$animal}-icon {{/1}}{{/4}}{{/7}}{{/10}}{
    {{2}}{{5}}{{8}}{{11}}background-image{{/2}}{{/5}}{{/8}}{{/11}}: {{3}}{{6}}{{9}}{{12}}url('/images/#{$animal}.png'){{/3}}{{/6}}{{/9}}{{/12}};
  }
}
SCSS
{{1}}.puma-icon{{/1}} {
  {{2}}background-image{{/2}}: {{3}}url("/images/puma.png"){{/3}}; }

{{4}}.sea-slug-icon{{/4}} {
  {{5}}background-image{{/5}}: {{6}}url("/images/sea-slug.png"){{/6}}; }

{{7}}.egret-icon{{/7}} {
  {{8}}background-image{{/8}}: {{9}}url("/images/egret.png"){{/9}}; }

{{10}}.salamander-icon{{/10}} {
  {{11}}background-image{{/11}}: {{12}}url("/images/salamander.png"){{/12}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_each_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@each $animal in puma, sea-slug, egret, salamander
  {{1}}{{4}}{{7}}{{10}}.#{$animal}-icon{{/1}}{{/4}}{{/7}}{{/10}}
    {{2}}{{5}}{{8}}{{11}}background-image{{/2}}{{/5}}{{/8}}{{/11}}: {{3}}{{6}}{{9}}{{12}}url('/images/#{$animal}.png'){{/3}}{{/6}}{{/9}}{{/12}}
SASS
{{1}}.puma-icon{{/1}} {
  {{2}}background-image{{/2}}: {{3}}url("/images/puma.png"){{/3}}; }

{{4}}.sea-slug-icon{{/4}} {
  {{5}}background-image{{/5}}: {{6}}url("/images/sea-slug.png"){{/6}}; }

{{7}}.egret-icon{{/7}} {
  {{8}}background-image{{/8}}: {{9}}url("/images/egret.png"){{/9}}; }

{{10}}.salamander-icon{{/10}} {
  {{11}}background-image{{/11}}: {{12}}url("/images/salamander.png"){{/12}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_mixin_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@mixin large-text {
  font: {
    {{2}}size{{/2}}: {{3}}20px{{/3}};
    {{4}}weight{{/4}}: {{5}}bold{{/5}};
  }
  {{6}}color{{/6}}: {{7}}#ff0000{{/7}};
}

{{1}}.page-title {{/1}}{
  @include large-text;
  {{8}}padding{{/8}}: {{9}}4px{{/9}};
}

@mixin dashed-border($color, $width: {{14}}1in{{/14}}) {
  border: {
    {{11}}{{18}}color{{/11}}{{/18}}: $color;
    {{13}}{{20}}width{{/13}}{{/20}}: $width;
    {{15}}{{22}}style{{/15}}{{/22}}: {{16}}{{23}}dashed{{/16}}{{/23}};
  }
}

{{10}}p {{/10}}{ @include dashed-border({{12}}blue{{/12}}); }
{{17}}h1 {{/17}}{ @include dashed-border({{19}}blue{{/19}}, {{21}}2in{{/21}}); }

@mixin box-shadow($shadows...) {
  {{25}}-moz-box-shadow{{/25}}: {{26}}$shadows{{/26}};
  {{27}}-webkit-box-shadow{{/27}}: {{28}}$shadows{{/28}};
  {{29}}box-shadow{{/29}}: {{30}}$shadows{{/30}};
}

{{24}}.shadows {{/24}}{
  @include box-shadow(0px 4px 5px #666, 2px 6px 10px #999);
}
SCSS
{{1}}.page-title{{/1}} {
  {{2}}font-size{{/2}}: {{3}}20px{{/3}};
  {{4}}font-weight{{/4}}: {{5}}bold{{/5}};
  {{6}}color{{/6}}: {{7}}#ff0000{{/7}};
  {{8}}padding{{/8}}: {{9}}4px{{/9}}; }

{{10}}p{{/10}} {
  {{11}}border-color{{/11}}: {{12}}blue{{/12}};
  {{13}}border-width{{/13}}: {{14}}1in{{/14}};
  {{15}}border-style{{/15}}: {{16}}dashed{{/16}}; }

{{17}}h1{{/17}} {
  {{18}}border-color{{/18}}: {{19}}blue{{/19}};
  {{20}}border-width{{/20}}: {{21}}2in{{/21}};
  {{22}}border-style{{/22}}: {{23}}dashed{{/23}}; }

{{24}}.shadows{{/24}} {
  {{25}}-moz-box-shadow{{/25}}: {{26}}0px 4px 5px #666, 2px 6px 10px #999{{/26}};
  {{27}}-webkit-box-shadow{{/27}}: {{28}}0px 4px 5px #666, 2px 6px 10px #999{{/28}};
  {{29}}box-shadow{{/29}}: {{30}}0px 4px 5px #666, 2px 6px 10px #999{{/30}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

def test_mixin_sourcemap_sass
  assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
=large-text
  :font
    {{2}}size{{/2}}: {{3}}20px{{/3}}
    {{4}}weight{{/4}}: {{5}}bold{{/5}}
  {{6}}color{{/6}}: {{7}}#ff0000{{/7}}

{{1}}.page-title{{/1}}
  +large-text
  {{8}}padding{{/8}}: {{9}}4px{{/9}}

=dashed-border($color, $width: {{14}}1in{{/14}})
  border:
    {{11}}{{18}}color{{/11}}{{/18}}: $color
    {{13}}{{20}}width{{/13}}{{/20}}: $width
    {{15}}{{22}}style{{/15}}{{/22}}: {{16}}{{23}}dashed{{/16}}{{/23}}

{{10}}p{{/10}}
  +dashed-border({{12}}blue{{/12}})

{{17}}h1{{/17}}
  +dashed-border({{19}}blue{{/19}}, {{21}}2in{{/21}})

=box-shadow($shadows...)
  {{25}}-moz-box-shadow{{/25}}: {{26}}$shadows{{/26}}
  {{27}}-webkit-box-shadow{{/27}}: {{28}}$shadows{{/28}}
  {{29}}box-shadow{{/29}}: {{30}}$shadows{{/30}}

{{24}}.shadows{{/24}}
  +box-shadow(0px 4px 5px #666, 2px 6px 10px #999)
SASS
{{1}}.page-title{{/1}} {
  {{2}}font-size{{/2}}: {{3}}20px{{/3}};
  {{4}}font-weight{{/4}}: {{5}}bold{{/5}};
  {{6}}color{{/6}}: {{7}}#ff0000{{/7}};
  {{8}}padding{{/8}}: {{9}}4px{{/9}}; }

{{10}}p{{/10}} {
  {{11}}border-color{{/11}}: {{12}}blue{{/12}};
  {{13}}border-width{{/13}}: {{14}}1in{{/14}};
  {{15}}border-style{{/15}}: {{16}}dashed{{/16}}; }

{{17}}h1{{/17}} {
  {{18}}border-color{{/18}}: {{19}}blue{{/19}};
  {{20}}border-width{{/20}}: {{21}}2in{{/21}};
  {{22}}border-style{{/22}}: {{23}}dashed{{/23}}; }

{{24}}.shadows{{/24}} {
  {{25}}-moz-box-shadow{{/25}}: {{26}}0px 4px 5px #666, 2px 6px 10px #999{{/26}};
  {{27}}-webkit-box-shadow{{/27}}: {{28}}0px 4px 5px #666, 2px 6px 10px #999{{/28}};
  {{29}}box-shadow{{/29}}: {{30}}0px 4px 5px #666, 2px 6px 10px #999{{/30}}; }

/*# sourceMappingURL=test.css.map */
CSS
end

  def test_function_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$grid-width: 20px;
$gutter-width: 5px;

@function grid-width($n) {
  @return $n * $grid-width + ($n - 1) * $gutter-width;
}
{{1}}sidebar {{/1}}{ {{2}}width{{/2}}: {{3}}grid-width(5){{/3}}; }
SCSS
{{1}}sidebar{{/1}} {
  {{2}}width{{/2}}: {{3}}120px{{/3}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_function_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$grid-width: 20px
$gutter-width: 5px

@function grid-width($n)
  @return $n * $grid-width + ($n - 1) * $gutter-width

{{1}}sidebar{{/1}}
  {{2}}width{{/2}}: {{3}}grid-width(5){{/3}}
SASS
{{1}}sidebar{{/1}} {
  {{2}}width{{/2}}: {{3}}120px{{/3}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  # Regression tests

  def test_properties_sass
    assert_parses_with_mapping <<SASS, <<CSS, :syntax => :sass
{{1}}.foo{{/1}}
  :{{2}}name{{/2}} {{3}}value{{/3}}
  {{4}}name{{/4}}: {{5}}value{{/5}}
  :{{6}}name{{/6}}  {{7}}value{{/7}}
  {{8}}name{{/8}}:  {{9}}value{{/9}}
SASS
{{1}}.foo{{/1}} {
  {{2}}name{{/2}}: {{3}}value{{/3}};
  {{4}}name{{/4}}: {{5}}value{{/5}};
  {{6}}name{{/6}}: {{7}}value{{/7}};
  {{8}}name{{/8}}: {{9}}value{{/9}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_multiline_script_scss
    assert_parses_with_mapping <<SCSS, <<CSS, :syntax => :scss
$var: {{3}}foo +
    bar{{/3}}; {{1}}x {{/1}}{ {{2}}y{{/2}}: $var }
SCSS
{{1}}x{{/1}} {
  {{2}}y{{/2}}: {{3}}foobar{{/3}}; }

/*# sourceMappingURL=test.css.map */
CSS
  end

  def test_multiline_interpolation_source_range
    engine = Sass::Engine.new(<<-SCSS, :cache => false, :syntax => :scss)
p {
  filter: progid:DXImageTransform(
          '\#{123}');
}
SCSS

    interpolated = engine.to_tree.children.
      first.children.
      first.value.children[1]
    assert_equal "123", interpolated.to_sass
    range = interpolated.source_range
    assert_equal 3, range.start_pos.line
    assert_equal 14, range.start_pos.offset
    assert_equal 3, range.end_pos.line
    assert_equal 17, range.end_pos.offset
  end

  def test_sources_array_is_uri_escaped
    map = Sass::Source::Map.new
    importer = Sass::Importers::Filesystem.new('.')
    map.add(
      Sass::Source::Range.new(
        Sass::Source::Position.new(0, 0),
        Sass::Source::Position.new(0, 10),
        'source file.scss',
        importer),
      Sass::Source::Range.new(
        Sass::Source::Position.new(0, 0),
        Sass::Source::Position.new(0, 10),
        nil, nil))

    json = map.to_json(:css_path => 'output file.css', :sourcemap_path => 'output file.css.map')
    assert_equal json, <<JSON.rstrip
{
"version": 3,
"mappings": "DADD,UAAU",
"sources": ["source%20file.scss"],
"names": [],
"file": "output%20file.css"
}
JSON
  end

  private

  ANNOTATION_REGEX = /\{\{(\/?)([^}]+)\}\}/

  def build_ranges(text, file_name = nil)
    ranges = Hash.new {|h, k| h[k] = []}
    start_positions = {}
    text.split("\n").each_with_index do |line_text, line|
      line += 1 # lines shoud be 1-based
      while (match = line_text.match(ANNOTATION_REGEX))
        closing = !match[1].empty?
        name = match[2]
        match_offsets = match.offset(0)
        offset = match_offsets[0] + 1 # Offsets are 1-based in source maps.
        assert(!closing || start_positions[name], "Closing annotation #{name} found before opening one.")
        position = Sass::Source::Position.new(line, offset)
        if closing
          ranges[name] << Sass::Source::Range.new(
            start_positions[name], position, file_name,
            Sass::Importers::Filesystem.new('.'))
          start_positions.delete name
        else
          assert(!start_positions[name], "Overlapping range annotation #{name} encountered on line #{line}")
          start_positions[name] = position
        end
        line_text.slice!(match_offsets[0], match_offsets[1] - match_offsets[0])
      end
    end
    ranges
  end

  def build_mapping_from_annotations(source, css, source_file_name)
    source_ranges = build_ranges(source, source_file_name)
    target_ranges = build_ranges(css)
    map = Sass::Source::Map.new
    Sass::Util.flatten(source_ranges.map do |(name, sources)|
        assert(sources.length == 1, "#{sources.length} source ranges encountered for annotation #{name}")
        assert(target_ranges[name], "No target ranges for annotation #{name}")
        target_ranges[name].map {|target_range| [sources.first, target_range]}
      end, 1).
      sort_by {|(_, target)| [target.start_pos.line, target.start_pos.offset]}.
      each {|(s2, target)| map.add(s2, target)}
    map
  end

  def assert_parses_with_mapping(source, css, options={})
    options[:syntax] ||= :scss
    input_filename = filename_for_test(options[:syntax])
    mapping = build_mapping_from_annotations(source, css, input_filename)
    source.gsub!(ANNOTATION_REGEX, "")
    css.gsub!(ANNOTATION_REGEX, "")
    rendered, sourcemap = render_with_sourcemap(source, options)
    assert_equal css.rstrip, rendered.rstrip
    assert_sourcemaps_equal source, css, mapping, sourcemap
  end

  def assert_positions_equal(expected, actual, lines, message = nil)
    prefix = message ? message + ": " : ""
    expected_location = lines[expected.line - 1] + "\n" + ("-" * (expected.offset - 1)) + "^"
    actual_location = lines[actual.line - 1] + "\n" + ("-" * (actual.offset - 1)) + "^"
    assert_equal(expected.line, actual.line, prefix +
      "Expected #{expected.inspect}\n" +
      expected_location + "\n\n" +
      "But was #{actual.inspect}\n" +
      actual_location)
    assert_equal(expected.offset, actual.offset, prefix +
      "Expected #{expected.inspect}\n" +
      expected_location + "\n\n" +
      "But was #{actual.inspect}\n" +
      actual_location)
  end

  def assert_ranges_equal(expected, actual, lines, prefix)
    assert_positions_equal(expected.start_pos, actual.start_pos, lines, prefix + " start position")
    assert_positions_equal(expected.end_pos, actual.end_pos, lines, prefix + " end position")
    assert_equal(expected.file, actual.file)
  end

  def assert_sourcemaps_equal(source, css, expected, actual)
    assert_equal(expected.data.length, actual.data.length, <<MESSAGE)
Wrong number of mappings. Expected:
#{dump_sourcemap_as_expectation(source, css, expected).gsub(/^/, '| ')}

Actual:
#{dump_sourcemap_as_expectation(source, css, actual).gsub(/^/, '| ')}
MESSAGE
    source_lines = source.split("\n")
    css_lines = css.split("\n")
    expected.data.zip(actual.data) do |expected_mapping, actual_mapping|
      assert_ranges_equal(expected_mapping.input, actual_mapping.input, source_lines, "Input")
      assert_ranges_equal(expected_mapping.output, actual_mapping.output, css_lines, "Output")
    end
  end

  def assert_parses_with_sourcemap(source, css, sourcemap_json, options={})
    rendered, sourcemap = render_with_sourcemap(source, options)
    css_path = options[:output] || "test.css"
    sourcemap_path = Sass::Util.sourcemap_name(css_path)
    rendered_json = sourcemap.to_json(:css_path => css_path, :sourcemap_path => sourcemap_path, :type => options[:sourcemap])

    assert_equal css.rstrip, rendered.rstrip
    assert_equal sourcemap_json.rstrip, rendered_json
  end

  def render_with_sourcemap(source, options={})
    options[:syntax] ||= :scss
    munge_filename options
    engine = Sass::Engine.new(source, options)
    engine.options[:cache] = false
    sourcemap_path = Sass::Util.sourcemap_name(options[:output] || "test.css")
    engine.render_with_sourcemap File.basename(sourcemap_path)
  end

  def dump_sourcemap_as_expectation(source, css, sourcemap)
    mappings_to_annotations(source, sourcemap.data.map {|d| d.input}) + "\n\n" +
      "=" * 20 + " maps to:\n\n" +
      mappings_to_annotations(css, sourcemap.data.map {|d| d.output})
  end

  def mappings_to_annotations(source, ranges)
    additional_offsets = Hash.new(0)
    lines = source.split("\n")

    add_annotation = lambda do |pos, str|
      line_num = pos.line - 1
      line = lines[line_num]
      offset = pos.offset + additional_offsets[line_num] - 1
      line << " " * (offset - line.length) if offset > line.length
      line.insert(offset, str)
      additional_offsets[line_num] += str.length
    end

    ranges.each_with_index do |range, i|
      add_annotation[range.start_pos, "{{#{i + 1}}}"]
      add_annotation[range.end_pos, "{{/#{i + 1}}}"]
    end

    return lines.join("\n")
  end
end
