#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'

class SourcemapTest < Test::Unit::TestCase
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

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";EACE,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["test_simple_mapping_scss_inline.scss"],
"file": "test.css"
}
JSON
  end

  def test_simple_mapping_sass
    assert_parses_with_sourcemap <<SASS, <<CSS, <<JSON, :syntax => :sass
a
  foo: bar
  /* SOME COMMENT */
  font-size: 12px
SASS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";EACE,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["test_simple_mapping_sass_inline.sass"],
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

  def test_mapping_with_directory_sass
    options = {:filename => "sass/style.sass", :output => "css/style.css", :syntax => :sass}
    assert_parses_with_sourcemap <<SASS, <<CSS, <<JSON, options
a
  foo: bar
  /* SOME COMMENT */
  font-size: 12px
SASS
a {
  foo: bar;
  /* SOME COMMENT */
  font-size: 12px; }

/*@ sourceMappingURL=style.css.map */
CSS
{
"version": "3",
"mappings": ";EACE,GAAG,EAAE,GAAG;;EAER,SAAS,EAAE,IAAI",
"sources": ["..\\/sass\\/style.sass"],
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

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";;EACE,GAAG,EAAE,GAAG",
"sources": ["test_simple_charset_mapping_scss_inline.scss"],
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

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";;EACE,GAAG,EAAE,GAAG",
"sources": ["test_simple_charset_mapping_sass_inline.sass"],
"file": "test.css"
}
JSON
    end

    def test_different_charset_than_encoding_scss
      assert_parses_with_sourcemap(<<SCSS.force_encoding("IBM866"), <<CSS.force_encoding("IBM866"), <<JSON)
@charset "IBM866";
f\x86\x86 {
  \x86: b;
}
SCSS
@charset "IBM866";
f\x86\x86 {
  \x86: b; }

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";;EAEE,CAAC,EAAE,CAAC",
"sources": ["test_different_charset_than_encoding_scss_inline.scss"],
"file": "test.css"
}
JSON
    end

    def test_different_charset_than_encoding_sass
      assert_parses_with_sourcemap(<<SASS.force_encoding("IBM866"), <<CSS.force_encoding("IBM866"), <<JSON, :syntax => :sass)
@charset "IBM866"
f\x86\x86
  \x86: b
SASS
@charset "IBM866";
f\x86\x86 {
  \x86: b; }

/*@ sourceMappingURL=test.css.map */
CSS
{
"version": "3",
"mappings": ";;EAEE,CAAC,EAAE,CAAC",
"sources": ["test_different_charset_than_encoding_sass_inline.sass"],
"file": "test.css"
}
JSON
    end
  end

  def test_import_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@import {{1}}url(foo){{/1}},{{2}}url(moo)   {{/2}},       {{3}}url(bar) {{/3}};
SCSS
{{1}}@import url(foo){{/1}};
{{2}}@import url(moo){{/2}};
{{3}}@import url(bar){{/3}};

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_import_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@import {{1}}foo.css{{/1}}, {{2}}moo.css{{/2}},  {{3}}bar.css{{/3}}
SASS
{{1}}@import url(foo.css){{/1}};
{{2}}@import url(moo.css){{/2}};
{{3}}@import url(bar.css){{/3}};

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_interpolation_and_vars_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$te: "te";
$teal: {{4}}teal{{/4}};
p {
  {{1}}con#{$te}nt{{/1}}: {{2}}"I a#{$te} #{5 + 10} pies!"{{/2}};
  {{3}}color{{/3}}: $teal;
}

$name: foo;
$attr: border;
p.#{$name} {
  {{5}}#{$attr}-color{{/5}}: {{6}}blue{{/6}};
  $font-size: 12px;
  $line-height: 30px;
  {{7}}font{{/7}}: {{8}}#{$font-size}/#{$line-height}{{/8}};
}
SCSS
p {
  {{1}}content{{/1}}: {{2}}"I ate 15 pies!"{{/2}};
  {{3}}color{{/3}}: {{4}}teal{{/4}}; }

p.foo {
  {{5}}border-color{{/5}}: {{6}}blue{{/6}};
  {{7}}font{{/7}}: {{8}}12px/30px{{/8}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_interpolation_and_vars_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$te: "te"
$teal: {{4}}teal{{/4}}
p
  {{1}}con#{$te}nt{{/1}}: {{2}}"I a#{$te} #{5 + 10} pies!"{{/2}}
  {{3}}color{{/3}}: $teal

$name: foo
$attr: border
p.#{$name}
  {{5}}#{$attr}-color{{/5}}: {{6}}blue{{/6}}
  $font-size: 12px
  $line-height: 30px
  {{7}}font{{/7}}: {{8}}#{$font-size}/#{$line-height}{{/8}}
SASS
p {
  {{1}}content{{/1}}: {{2}}"I ate 15 pies!"{{/2}};
  {{3}}color{{/3}}: {{4}}teal{{/4}}; }

p.foo {
  {{5}}border-color{{/5}}: {{6}}blue{{/6}};
  {{7}}font{{/7}}: {{8}}12px/30px{{/8}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_selectors_properties_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$width: 2px;
$translucent-red: rgba(255, 0, 0, 0.5);
a {
  .special {
    {{7}}color{{/7}}: {{8}}red{{/8}};
    &:hover {
      {{9}}foo{{/9}}: {{10}}bar{{/10}};
      {{11}}cursor{{/11}}: {{12}}e + -resize{{/12}};
      {{13}}color{{/13}}: {{14}}opacify($translucent-red, 0.3){{/14}};
    }
    &:after {
      {{15}}content{{/15}}: {{16}}"I ate #{5 + 10} pies #{$width} thick!"{{/16}};
    }
  }
  &:active {
    {{17}}color{{/17}}: {{18}}#010203 + #040506{{/18}};
    {{19}}border{{/19}}: {{20}}$width solid black{{/20}};
  }
/* SOME COMMENT */
  {{1}}font{{/1}}: {{2}}2px/3px {{/2}}{
    {{3}}family{{/3}}: {{4}}fantasy{{/4}};
    {{5}}size{{/5}}: {{6}}1em + (2em * 3){{/6}};
  }
}
SCSS
a {
  /* SOME COMMENT */
  {{1}}font{{/1}}: {{2}}2px/3px{{/2}};
    {{3}}font-family{{/3}}: {{4}}fantasy{{/4}};
    {{5}}font-size{{/5}}: {{6}}7em{{/6}}; }
  a .special {
    {{7}}color{{/7}}: {{8}}red{{/8}}; }
    a .special:hover {
      {{9}}foo{{/9}}: {{10}}bar{{/10}};
      {{11}}cursor{{/11}}: {{12}}e-resize{{/12}};
      {{13}}color{{/13}}: {{14}}rgba(255, 0, 0, 0.8){{/14}}; }
    a .special:after {
      {{15}}content{{/15}}: {{16}}"I ate 15 pies 2px thick!"{{/16}}; }
  a:active {
    {{17}}color{{/17}}: {{18}}#050709{{/18}};
    {{19}}border{{/19}}: {{20}}2px solid black{{/20}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_selectors_properties_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$width: 2px
$translucent-red: rgba(255, 0, 0, 0.5)
a
  .special
    {{7}}color{{/7}}: {{8}}red{{/8}}
    &:hover
      {{9}}foo{{/9}}: {{10}}bar{{/10}}
      {{11}}cursor{{/11}}: {{12}}e + -resize{{/12}}
      {{13}}color{{/13}}: {{14}}opacify($translucent-red, 0.3){{/14}}
    &:after
      {{15}}content{{/15}}: {{16}}"I ate #{5 + 10} pies #{$width} thick!"{{/16}}
  &:active
    {{17}}color{{/17}}: {{18}}#010203 + #040506{{/18}}
    {{19}}border{{/19}}: {{20}}$width solid black{{/20}}

  /* SOME COMMENT */
  {{1}}font{{/1}}: {{2}}2px/3px{{/2}}
    {{3}}family{{/3}}: {{4}}fantasy{{/4}}
    {{5}}size{{/5}}: {{6}}1em + (2em * 3){{/6}}
SASS
a {
  /* SOME COMMENT */
  {{1}}font{{/1}}: {{2}}2px/3px{{/2}};
    {{3}}font-family{{/3}}: {{4}}fantasy{{/4}};
    {{5}}font-size{{/5}}: {{6}}7em{{/6}}; }
  a .special {
    {{7}}color{{/7}}: {{8}}red{{/8}}; }
    a .special:hover {
      {{9}}foo{{/9}}: {{10}}bar{{/10}};
      {{11}}cursor{{/11}}: {{12}}e-resize{{/12}};
      {{13}}color{{/13}}: {{14}}rgba(255, 0, 0, 0.8){{/14}}; }
    a .special:after {
      {{15}}content{{/15}}: {{16}}"I ate 15 pies 2px thick!"{{/16}}; }
  a:active {
    {{17}}color{{/17}}: {{18}}#050709{{/18}};
    {{19}}border{{/19}}: {{20}}2px solid black{{/20}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_extend_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
.error {
  {{1}}border{{/1}}: {{2}}1px #f00{{/2}};
  {{3}}background-color{{/3}}: {{4}}#fdd{{/4}};
}
.seriousError {
  @extend .error;
  {{5}}border-width{{/5}}: {{6}}3px{{/6}};
}
SCSS
.error, .seriousError {
  {{1}}border{{/1}}: {{2}}1px #f00{{/2}};
  {{3}}background-color{{/3}}: {{4}}#fdd{{/4}}; }

.seriousError {
  {{5}}border-width{{/5}}: {{6}}3px{{/6}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_extend_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
.error
  {{1}}border{{/1}}: {{2}}1px #f00{{/2}}
  {{3}}background-color{{/3}}: {{4}}#fdd{{/4}}

.seriousError
  @extend .error
  {{5}}border-width{{/5}}: {{6}}3px{{/6}}
SASS
.error, .seriousError {
  {{1}}border{{/1}}: {{2}}1px red{{/2}};
  {{3}}background-color{{/3}}: {{4}}#ffdddd{{/4}}; }

.seriousError {
  {{5}}border-width{{/5}}: {{6}}3px{{/6}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_for_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@for $i from 1 through 3 {
  .item-#{$i} { {{1}}width{{/1}}: {{2}}2em * $i{{/2}}; }
}
SCSS
.item-1 {
  {{1}}width{{/1}}: {{2}}2em{{/2}}; }

.item-2 {
  {{1}}width{{/1}}: {{2}}4em{{/2}}; }

.item-3 {
  {{1}}width{{/1}}: {{2}}6em{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_for_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@for $i from 1 through 3
  .item-#{$i}
    {{1}}width{{/1}}: {{2}}2em * $i{{/2}}
SASS
.item-1 {
  {{1}}width{{/1}}: {{2}}2em{{/2}}; }

.item-2 {
  {{1}}width{{/1}}: {{2}}4em{{/2}}; }

.item-3 {
  {{1}}width{{/1}}: {{2}}6em{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_while_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$i: 6;
@while $i > 0 {
  .item-#{$i} { {{1}}width{{/1}}: {{2}}2em * $i{{/2}}; }
  $i: $i - 2;
}
SCSS
.item-6 {
  {{1}}width{{/1}}: {{2}}12em{{/2}}; }

.item-4 {
  {{1}}width{{/1}}: {{2}}8em{{/2}}; }

.item-2 {
  {{1}}width{{/1}}: {{2}}4em{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

def test_while_sourcemap_sass
  assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$i: 6
@while $i > 0
  .item-#{$i}
    {{1}}width{{/1}}: {{2}}2em * $i{{/2}}
  $i: $i - 2
SASS
.item-6 {
  {{1}}width{{/1}}: {{2}}12em{{/2}}; }

.item-4 {
  {{1}}width{{/1}}: {{2}}8em{{/2}}; }

.item-2 {
  {{1}}width{{/1}}: {{2}}4em{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_each_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@each $animal in puma, sea-slug, egret, salamander {
  .#{$animal}-icon {
    {{1}}background-image{{/1}}: {{2}}url('/images/#{$animal}.png'){{/2}};
  }
}
SCSS
.puma-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/puma.png"){{/2}}; }

.sea-slug-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/sea-slug.png"){{/2}}; }

.egret-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/egret.png"){{/2}}; }

.salamander-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/salamander.png"){{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_each_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
@each $animal in puma, sea-slug, egret, salamander
  .#{$animal}-icon
    {{1}}background-image{{/1}}: {{2}}url('/images/#{$animal}.png'){{/2}}
SASS
.puma-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/puma.png"){{/2}}; }

.sea-slug-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/sea-slug.png"){{/2}}; }

.egret-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/egret.png"){{/2}}; }

.salamander-icon {
  {{1}}background-image{{/1}}: {{2}}url("/images/salamander.png"){{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_mixin_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
@mixin large-text {
  font: {
    {{1}}size{{/1}}: {{2}}20px{{/2}};
    {{3}}weight{{/3}}: {{4}}bold{{/4}};
  }
  {{5}}color{{/5}}: {{6}}#ff0000{{/6}};
}

.page-title {
  @include large-text;
  {{7}}padding{{/7}}: {{8}}4px{{/8}};
}

@mixin dashed-border($color, $width: {{24}}1in{{/24}}) {
  border: {
    {{9}}color{{/9}}: $color;
    {{11}}width{{/11}}: $width;
    {{13}}style{{/13}}: {{14}}dashed{{/14}};
  }
}

p { @include dashed-border({{10}}blue{{/10}}); }
h1 { @include dashed-border({{25}}blue{{/25}}, {{26}}2in{{/26}}); }

@mixin box-shadow($shadows...) {
  {{18}}-moz-box-shadow{{/18}}: {{19}}$shadows{{/19}};
  {{20}}-webkit-box-shadow{{/20}}: {{21}}$shadows{{/21}};
  {{22}}box-shadow{{/22}}: {{23}}$shadows{{/23}};
}

.shadows {
  @include box-shadow(0px 4px 5px #666, 2px 6px 10px #999);
}
SCSS
.page-title {
  {{1}}font-size{{/1}}: {{2}}20px{{/2}};
  {{3}}font-weight{{/3}}: {{4}}bold{{/4}};
  {{5}}color{{/5}}: {{6}}#ff0000{{/6}};
  {{7}}padding{{/7}}: {{8}}4px{{/8}}; }

p {
  {{9}}border-color{{/9}}: {{10}}blue{{/10}};
  {{11}}border-width{{/11}}: {{24}}1in{{/24}};
  {{13}}border-style{{/13}}: {{14}}dashed{{/14}}; }

h1 {
  {{9}}border-color{{/9}}: {{25}}blue{{/25}};
  {{11}}border-width{{/11}}: {{26}}2in{{/26}};
  {{13}}border-style{{/13}}: {{14}}dashed{{/14}}; }

.shadows {
  {{18}}-moz-box-shadow{{/18}}: {{19}}0px 4px 5px #666666, 2px 6px 10px #999999{{/19}};
  {{20}}-webkit-box-shadow{{/20}}: {{21}}0px 4px 5px #666666, 2px 6px 10px #999999{{/21}};
  {{22}}box-shadow{{/22}}: {{23}}0px 4px 5px #666666, 2px 6px 10px #999999{{/23}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

def test_mixin_sourcemap_sass
  assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
=large-text
  :font
    {{1}}size{{/1}}: {{2}}20px{{/2}}
    {{3}}weight{{/3}}: {{4}}bold{{/4}}
  {{5}}color{{/5}}: {{6}}#ff0000{{/6}}

.page-title
  +large-text
  {{7}}padding{{/7}}: {{8}}4px{{/8}}

=dashed-border($color, $width: {{24}}1in{{/24}})
  border:
    {{9}}color{{/9}}: $color
    {{11}}width{{/11}}: $width
    {{13}}style{{/13}}: {{14}}dashed{{/14}}

p
  +dashed-border({{10}}blue{{/10}})

h1
  +dashed-border({{25}}blue{{/25}}, {{26}}2in{{/26}})

=box-shadow($shadows...)
  {{18}}-moz-box-shadow{{/18}}: {{19}}$shadows{{/19}}
  {{20}}-webkit-box-shadow{{/20}}: {{21}}$shadows{{/21}}
  {{22}}box-shadow{{/22}}: {{23}}$shadows{{/23}}

.shadows
  +box-shadow(0px 4px 5px #666, 2px 6px 10px #999)
SASS
.page-title {
  {{1}}font-size{{/1}}: {{2}}20px{{/2}};
  {{3}}font-weight{{/3}}: {{4}}bold{{/4}};
  {{5}}color{{/5}}: {{6}}red{{/6}};
  {{7}}padding{{/7}}: {{8}}4px{{/8}}; }

p {
  {{9}}border-color{{/9}}: {{10}}blue{{/10}};
  {{11}}border-width{{/11}}: {{24}}1in{{/24}};
  {{13}}border-style{{/13}}: {{14}}dashed{{/14}}; }

h1 {
  {{9}}border-color{{/9}}: {{25}}blue{{/25}};
  {{11}}border-width{{/11}}: {{26}}2in{{/26}};
  {{13}}border-style{{/13}}: {{14}}dashed{{/14}}; }

.shadows {
  {{18}}-moz-box-shadow{{/18}}: {{19}}0px 4px 5px #666666, 2px 6px 10px #999999{{/19}};
  {{20}}-webkit-box-shadow{{/20}}: {{21}}0px 4px 5px #666666, 2px 6px 10px #999999{{/21}};
  {{22}}box-shadow{{/22}}: {{23}}0px 4px 5px #666666, 2px 6px 10px #999999{{/23}}; }

/*@ sourceMappingURL=test.css.map */
CSS
end

  def test_function_sourcemap_scss
    assert_parses_with_mapping <<'SCSS', <<'CSS'
$grid-width: 20px;
$gutter-width: 5px;

@function grid-width($n) {
  @return $n * $grid-width + ($n - 1) * $gutter-width;
}
sidebar { {{1}}width{{/1}}: {{2}}grid-width(5){{/2}}; }
SCSS
sidebar {
  {{1}}width{{/1}}: {{2}}120px{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  def test_function_sourcemap_sass
    assert_parses_with_mapping <<'SASS', <<'CSS', :syntax => :sass
$grid-width: 20px
$gutter-width: 5px

@function grid-width($n)
  @return $n * $grid-width + ($n - 1) * $gutter-width

sidebar
  {{1}}width{{/1}}: {{2}}grid-width(5){{/2}}
SASS
sidebar {
  {{1}}width{{/1}}: {{2}}120px{{/2}}; }

/*@ sourceMappingURL=test.css.map */
CSS
  end

  @private

  ANNOTATION_REGEX = /\{\{(\/?)([^}]+)\}\}/

  def build_ranges(text, file_name = nil)
    ranges = Hash.new {|h, k| h[k] = []}
    start_positions = {}
    text.split("\n").each_with_index do |line_text, line|
      line += 1 # lines shoud be 1-based
      match_start = 0
      while match = line_text.match(ANNOTATION_REGEX)
        closing = !match[1].empty?
        name = match[2]
        match_offsets = match.offset(0)
        offset = match_offsets[0] + 1 # Offsets are 1-based in source maps.
        assert(!closing || start_positions[name], "Closing annotation #{name} found before opening one.")
        position = Sass::Source::Position.new(line, offset)
        if closing
          ranges[name] << Sass::Source::Range.new(start_positions[name], position, file_name)
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

  def build_mapping_from_annotations(scss, css, source_file_name)
    source_ranges = build_ranges(scss, source_file_name)
    target_ranges = build_ranges(css)
    map = Sass::Source::Map.new
    mappings = Sass::Util.flatten(source_ranges.map do |(name, sources)|
        assert(sources.length == 1, "#{sources.length} source ranges encountered for annotation #{name}")
        assert(target_ranges[name], "No target ranges for annotation #{name}")
        target_ranges[name].map {|target_range| [sources.first, target_range]}
      end, 1).
      sort_by {|(source, target)| [target.start_pos.line, target.start_pos.offset]}.
      each {|(source, target)| map.add(source, target)}
    map
  end

  def assert_parses_with_mapping(input, css, options={})
    options[:syntax] ||= :scss
    input_filename = filename_for_test(options[:syntax])
    mapping = build_mapping_from_annotations(input, css, input_filename)
    input.gsub!(ANNOTATION_REGEX, "")
    css.gsub!(ANNOTATION_REGEX, "")
    rendered, sourcemap = render_with_sourcemap(input, options)
    assert_equal css.rstrip, rendered.rstrip
    assert_sourcemaps_equal input, css, mapping, sourcemap
  end

  def assert_positions_equal(expected, actual, lines, message = nil)
    prefix = message ? message + ": " : ""
    assert_equal(expected.line, actual.line, prefix +
      "Expected #{expected.inspect} but was #{actual.inspect}")
    assert_equal(expected.offset, actual.offset, prefix +
      "Expected #{expected.inspect} but was #{actual.inspect}\n" +
      lines[actual.line - 1] + "\n" + ("-" * (actual.offset - 1)) + "^")
  end

  def assert_ranges_equal(expected, actual, lines, prefix)
    assert_positions_equal(expected.start_pos, actual.start_pos, lines, prefix + " start position")
    assert_positions_equal(expected.end_pos, actual.end_pos, lines, prefix + " end position")
    assert_equal(expected.file, actual.file)
  end

  def assert_sourcemaps_equal(scss, css, expected, actual)
    assert_equal(expected.data.length, actual.data.length, dump_sourcemap_as_expectation(actual))
    scss_lines = scss.split(/\n/)
    css_lines = css.split(/\n/)
    expected.data.zip(actual.data) do |expected_mapping, actual_mapping|
      assert_ranges_equal(expected_mapping.input, actual_mapping.input, scss_lines, "Input")
      assert_ranges_equal(expected_mapping.output, actual_mapping.output, css_lines, "Output")
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

  def dump_sourcemap_as_expectation(sourcemap)
    sourcemap.data.map do |mapping|
      input_start_pos = mapping.input.start_pos;
      input_end_pos = mapping.input.end_pos;
      output_start_pos = mapping.output.start_pos;
      output_end_pos = mapping.output.end_pos;
      "[#{input_start_pos.line}, #{input_start_pos.offset}, #{input_end_pos.line}, #{input_end_pos.offset}], " +
        "[#{output_start_pos.line}, #{output_start_pos.offset}, #{output_end_pos.line}, #{output_end_pos.offset}]"
    end.join(",\n") + "\n"
  end
end
