#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
require File.dirname(__FILE__) + '/../test_helper'

class EngineTest < Test::Unit::TestCase
  # A map of erroneous Haml documents to the error messages they should produce.
  # The error messages may be arrays;
  # if so, the second element should be the line number that should be reported for the error.
  # If this isn't provided, the tests will assume the line number should be the last line of the document.
  EXCEPTION_MAP = {
    "!!!\n  a" => "Illegal nesting: nesting within a header command is illegal.",
    "a\n  b" => "Illegal nesting: nesting within plain text is illegal.",
    "/ a\n  b" => "Illegal nesting: nesting within a tag that already has content is illegal.",
    "% a" => 'Invalid tag: "% a".',
    "%p a\n  b" => "Illegal nesting: content can't be both given on the same line as %p and nested within it.",
    "%p=" => "There's no Ruby code for = to evaluate.",
    "%p~" => "There's no Ruby code for ~ to evaluate.",
    "~" => "There's no Ruby code for ~ to evaluate.",
    "=" => "There's no Ruby code for = to evaluate.",
    "%p/\n  a" => "Illegal nesting: nesting within a self-closing tag is illegal.",
    ":a\n  b" => ['Filter "a" is not defined.', 1],
    ":a= b" => 'Invalid filter name ":a= b".',
    "." => "Illegal element: classes and ids must have values.",
    ".#" => "Illegal element: classes and ids must have values.",
    ".{} a" => "Illegal element: classes and ids must have values.",
    ".() a" => "Illegal element: classes and ids must have values.",
    ".= a" => "Illegal element: classes and ids must have values.",
    "%p..a" => "Illegal element: classes and ids must have values.",
    "%a/ b" => "Self-closing tags can't have content.",
    "%p{:a => 'b',\n:c => 'd'}/ e" => ["Self-closing tags can't have content.", 2],
    "%p{:a => 'b',\n:c => 'd'}=" => ["There's no Ruby code for = to evaluate.", 2],
    "%p.{:a => 'b',\n:c => 'd'} e" => ["Illegal element: classes and ids must have values.", 1],
    "%p{:a => 'b',\n:c => 'd',\n:e => 'f'}\n%p/ a" => ["Self-closing tags can't have content.", 4],
    "%p{:a => 'b',\n:c => 'd',\n:e => 'f'}\n- raise 'foo'" => ["foo", 4],
    "%p{:a => 'b',\n:c => raise('foo'),\n:e => 'f'}" => ["foo", 2],
    "%p{:a => 'b',\n:c => 'd',\n:e => raise('foo')}" => ["foo", 3],
    " %p foo" => "Indenting at the beginning of the document is illegal.",
    "  %p foo" => "Indenting at the beginning of the document is illegal.",
    "- end" => <<MESSAGE.rstrip,
You don't need to use "- end" in Haml. Un-indent to close a block:
- if foo?
  %strong Foo!
- else
  Not foo.
%p This line is un-indented, so it isn't part of the "if" block
MESSAGE
    " \n\t\n %p foo" => ["Indenting at the beginning of the document is illegal.", 3],
    "\n\n %p foo" => ["Indenting at the beginning of the document is illegal.", 3],
    "%p\n  foo\n foo" => ["Inconsistent indentation: 1 space was used for indentation, but the rest of the document was indented using 2 spaces.", 3],
    "%p\n  foo\n%p\n foo" => ["Inconsistent indentation: 1 space was used for indentation, but the rest of the document was indented using 2 spaces.", 4],
    "%p\n\t\tfoo\n\tfoo" => ["Inconsistent indentation: 1 tab was used for indentation, but the rest of the document was indented using 2 tabs.", 3],
    "%p\n  foo\n   foo" => ["Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces.", 3],
    "%p\n  foo\n  %p\n   bar" => ["Inconsistent indentation: 3 spaces were used for indentation, but the rest of the document was indented using 2 spaces.", 4],
    "%p\n  :plain\n     bar\n   \t  baz" => ['Inconsistent indentation: "   \t  " was used for indentation, but the rest of the document was indented using 2 spaces.', 4],
    "%p\n  foo\n%p\n    bar" => ["The line was indented 2 levels deeper than the previous line.", 4],
    "%p\n  foo\n  %p\n        bar" => ["The line was indented 3 levels deeper than the previous line.", 4],
    "%p\n \tfoo" => ["Indentation can't use both tabs and spaces.", 2],
    "%p(" => "Invalid attribute list: \"(\".",
    "%p(foo=\nbar)" => ["Invalid attribute list: \"(foo=\".", 1],
    "%p(foo=)" => "Invalid attribute list: \"(foo=)\".",
    "%p(foo 'bar')" => "Invalid attribute list: \"(foo 'bar')\".",
    "%p(foo 'bar'\nbaz='bang')" => ["Invalid attribute list: \"(foo 'bar'\".", 1],
    "%p(foo='bar'\nbaz 'bang'\nbip='bop')" => ["Invalid attribute list: \"(foo='bar' baz 'bang'\".", 2],
    "%p{:foo => 'bar' :bar => 'baz'}" => :compile,
    "%p{:foo => }" => :compile,
    "%p{=> 'bar'}" => :compile,
    "%p{:foo => 'bar}" => :compile,
    "%p{'foo => 'bar'}" => :compile,
    "%p{:foo => 'bar\"}" => :compile,

    # Regression tests
    "- raise 'foo'\n\n\n\nbar" => ["foo", 1],
    "= 'foo'\n-raise 'foo'" => ["foo", 2],
    "\n\n\n- raise 'foo'" => ["foo", 4],
    "%p foo |\n   bar |\n   baz |\nbop\n- raise 'foo'" => ["foo", 5],
    "foo\n\n\n  bar" => ["Illegal nesting: nesting within plain text is illegal.", 4],
    "%p/\n\n  bar" => ["Illegal nesting: nesting within a self-closing tag is illegal.", 3],
    "%p foo\n\n  bar" => ["Illegal nesting: content can't be both given on the same line as %p and nested within it.", 3],
    "/ foo\n\n  bar" => ["Illegal nesting: nesting within a tag that already has content is illegal.", 3],
    "!!!\n\n  bar" => ["Illegal nesting: nesting within a header command is illegal.", 3],
    "foo\n:ruby\n  1\n  2\n  3\n- raise 'foo'" => ["foo", 6],
    "foo\n:erb\n  1\n  2\n  3\n- raise 'foo'" => ["foo", 6],
    "foo\n:plain\n  1\n  2\n  3\n- raise 'foo'" => ["foo", 6],
    "foo\n:plain\n  1\n  2\n  3\n4\n- raise 'foo'" => ["foo", 7],
    "foo\n:plain\n  1\n  2\n  3\#{''}\n- raise 'foo'" => ["foo", 6],
    "foo\n:plain\n  1\n  2\n  3\#{''}\n4\n- raise 'foo'" => ["foo", 7],
    "foo\n:plain\n  1\n  2\n  \#{raise 'foo'}" => ["foo", 5],
    "= raise 'foo'\nfoo\nbar\nbaz\nbang" => ["foo", 1],
  }

  User = Struct.new('User', :id)
  class CustomHamlClass < Struct.new(:id)
    def haml_object_ref
      "my_thing"
    end
  end

  def render(text, options = {}, &block)
    scope  = options.delete(:scope)  || Object.new
    locals = options.delete(:locals) || {}
    engine(text, options).to_html(scope, locals, &block)
  end
  
  def engine(text, options = {})
    unless options[:filename]
      # use caller method name as fake filename. useful for debugging
      i = -1
      caller[i+=1] =~ /`(.+?)'/ until $1 and $1.index('test_') == 0
      options[:filename] = "(#{$1})"
    end
    Haml::Engine.new(text, options)
  end

  def test_empty_render
    assert_equal "", render("")
  end

  def test_flexible_tabulation
    assert_equal("<p>\n  foo\n</p>\n<q>\n  bar\n  <a>\n    baz\n  </a>\n</q>\n",
                 render("%p\n foo\n%q\n bar\n %a\n  baz"))
    assert_equal("<p>\n  foo\n</p>\n<q>\n  bar\n  <a>\n    baz\n  </a>\n</q>\n",
                 render("%p\n\tfoo\n%q\n\tbar\n\t%a\n\t\tbaz"))
    assert_equal("<p>\n      \t \t bar\n   baz\n</p>\n",
                 render("%p\n  :plain\n        \t \t bar\n     baz"))
  end

  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  def test_attributes_should_render_correctly
    assert_equal("<div class='atlantis' style='ugly'></div>", render(".atlantis{:style => 'ugly'}").chomp)
  end

  def test_css_id_as_attribute_should_be_appended_with_underscore
    assert_equal("<div id='my_id_1'></div>", render("#my_id{:id => '1'}").chomp)
    assert_equal("<div id='my_id_1'></div>", render("#my_id{:id => 1}").chomp)
  end

  def test_ruby_code_should_work_inside_attributes
    author = 'hcatlin'
    assert_equal("<p class='3'>foo</p>", render("%p{:class => 1+2} foo").chomp)
  end

  def test_class_attr_with_array
    assert_equal("<p class='a b'>foo</p>\n", render("%p{:class => %w[a b]} foo")) # basic
    assert_equal("<p class='a b css'>foo</p>\n", render("%p.css{:class => %w[a b]} foo")) # merge with css
    assert_equal("<p class='b css'>foo</p>\n", render("%p.css{:class => %w[css b]} foo")) # merge uniquely
    assert_equal("<p class='a b c d'>foo</p>\n", render("%p{:class => [%w[a b], %w[c d]]} foo")) # flatten
    assert_equal("<p class='a b'>foo</p>\n", render("%p{:class => [:a, :b] } foo")) # stringify
    assert_equal("<p class=''>foo</p>\n", render("%p{:class => [nil, false] } foo")) # strip falsey
    assert_equal("<p class='a'>foo</p>\n", render("%p{:class => :a} foo")) # single stringify
    assert_equal("<p>foo</p>\n", render("%p{:class => false} foo")) # single falsey
    assert_equal("<p class='a b html'>foo</p>\n", render("%p(class='html'){:class => %w[a b]} foo")) # html attrs
  end

  def test_id_attr_with_array
    assert_equal("<p id='a_b'>foo</p>\n", render("%p{:id => %w[a b]} foo")) # basic
    assert_equal("<p id='css_a_b'>foo</p>\n", render("%p#css{:id => %w[a b]} foo")) # merge with css
    assert_equal("<p id='a_b_c_d'>foo</p>\n", render("%p{:id => [%w[a b], %w[c d]]} foo")) # flatten
    assert_equal("<p id='a_b'>foo</p>\n", render("%p{:id => [:a, :b] } foo")) # stringify
    assert_equal("<p id=''>foo</p>\n", render("%p{:id => [nil, false] } foo")) # strip falsey
    assert_equal("<p id='a'>foo</p>\n", render("%p{:id => :a} foo")) # single stringify
    assert_equal("<p>foo</p>\n", render("%p{:id => false} foo")) # single falsey
    assert_equal("<p id='html_a_b'>foo</p>\n", render("%p(id='html'){:id => %w[a b]} foo")) # html attrs
  end

  def test_colon_in_class_attr
    assert_equal("<p class='foo:bar' />\n", render("%p.foo:bar/"))
  end

  def test_colon_in_id_attr
    assert_equal("<p id='foo:bar' />\n", render("%p#foo:bar/"))
  end

  def test_dynamic_attributes_with_no_content
    assert_equal(<<HTML, render(<<HAML))
<p>
  <a href='http://haml-lang.com'></a>
</p>
HTML
%p
  %a{:href => "http://" + "haml-lang.com"}
HAML
  end

  def test_attributes_with_to_s
    assert_equal(<<HTML, render(<<HAML))
<p id='foo_2'></p>
<p class='2 foo'></p>
<p blaz='2'></p>
<p 2='2'></p>
HTML
%p#foo{:id => 1+1}
%p.foo{:class => 1+1}
%p{:blaz => 1+1}
%p{(1+1) => 1+1}
HAML
  end

  def test_nil_should_render_empty_tag
    assert_equal("<div class='no_attributes'></div>",
                 render(".no_attributes{:nil => nil}").chomp)
  end

  def test_strings_should_get_stripped_inside_tags
    assert_equal("<div class='stripped'>This should have no spaces in front of it</div>",
                 render(".stripped    This should have no spaces in front of it").chomp)
  end

  def test_one_liner_should_be_one_line
    assert_equal("<p>Hello</p>", render('%p Hello').chomp)
  end

  def test_one_liner_with_newline_shouldnt_be_one_line
    assert_equal("<p>\n  foo\n  bar\n</p>", render('%p= "foo\nbar"').chomp)
  end

  def test_multi_render
    engine = engine("%strong Hi there!")
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
    assert_equal("<strong>Hi there!</strong>\n", engine.to_html)
  end

  def test_interpolation
    assert_equal("<p>Hello World</p>\n", render('%p Hello #{who}', :locals => {:who => 'World'}))
    assert_equal("<p>\n  Hello World\n</p>\n", render("%p\n  Hello \#{who}", :locals => {:who => 'World'}))
  end

  def test_interpolation_in_the_middle_of_a_string
    assert_equal("\"title 'Title'. \"\n",
                 render("\"title '\#{\"Title\"}'. \""))
  end

  def test_interpolation_at_the_beginning_of_a_line
    assert_equal("<p>2</p>\n", render('%p #{1 + 1}'))
    assert_equal("<p>\n  2\n</p>\n", render("%p\n  \#{1 + 1}"))
  end

  def test_escaped_interpolation
    assert_equal("<p>Foo &amp; Bar & Baz</p>\n", render('%p& Foo #{"&"} Bar & Baz'))
  end

  def test_nil_tag_value_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= nil"))
  end

  def test_tag_with_failed_if_should_render_as_empty
    assert_equal("<p></p>\n", render("%p= 'Hello' if false"))
  end

  def test_static_attributes_with_empty_attr
    assert_equal("<img alt='' src='/foo.png' />\n", render("%img{:src => '/foo.png', :alt => ''}"))
  end

  def test_dynamic_attributes_with_empty_attr
    assert_equal("<img alt='' src='/foo.png' />\n", render("%img{:width => nil, :src => '/foo.png', :alt => String.new}"))
  end

  def test_attribute_hash_with_newlines
    assert_equal("<p a='b' c='d'>foop</p>\n", render("%p{:a => 'b',\n   :c => 'd'} foop"))
    assert_equal("<p a='b' c='d'>\n  foop\n</p>\n", render("%p{:a => 'b',\n   :c => 'd'}\n  foop"))
    assert_equal("<p a='b' c='d' />\n", render("%p{:a => 'b',\n   :c => 'd'}/"))
    assert_equal("<p a='b' c='d' e='f'></p>\n", render("%p{:a => 'b',\n   :c => 'd',\n   :e => 'f'}"))
  end

  def test_attr_hashes_not_modified
    hash = {:color => 'red'}
    assert_equal(<<HTML, render(<<HAML, :locals => {:hash => hash}))
<div color='red'></div>
<div class='special' color='red'></div>
<div color='red'></div>
HTML
%div{hash}
.special{hash}
%div{hash}
HAML
    assert_equal(hash, {:color => 'red'})
  end

  def test_end_of_file_multiline
    assert_equal("<p>0</p>\n<p>1</p>\n<p>2</p>\n", render("- for i in (0...3)\n  %p= |\n   i |"))
  end

  def test_cr_newline
    assert_equal("<p>foo</p>\n<p>bar</p>\n<p>baz</p>\n<p>boom</p>\n", render("%p foo\r%p bar\r\n%p baz\n\r%p boom"))
  end

  def test_textareas
    assert_equal("<textarea>Foo&#x000A;  bar&#x000A;   baz</textarea>\n",
                 render('%textarea= "Foo\n  bar\n   baz"'))

    assert_equal("<pre>Foo&#x000A;  bar&#x000A;   baz</pre>\n",
                 render('%pre= "Foo\n  bar\n   baz"'))

    assert_equal("<textarea>#{'a' * 100}</textarea>\n",
                 render("%textarea #{'a' * 100}"))

    assert_equal("<p>\n  <textarea>Foo\n  Bar\n  Baz</textarea>\n</p>\n", render(<<SOURCE))
%p
  %textarea
    Foo
    Bar
    Baz
SOURCE
  end

  def test_pre_code
    assert_equal(<<HTML, render(<<HAML))
<pre><code>Foo&#x000A;  bar&#x000A;    baz</code></pre>
HTML
%pre
  %code
    :preserve
      Foo
        bar
          baz
HAML
  end

  def test_boolean_attributes
    assert_equal("<p bar baz='true' foo='bar'></p>\n",
                 render("%p{:foo => 'bar', :bar => true, :baz => 'true'}", :format => :html4))
    assert_equal("<p bar='bar' baz='true' foo='bar'></p>\n",
                 render("%p{:foo => 'bar', :bar => true, :baz => 'true'}", :format => :xhtml))

    assert_equal("<p baz='false' foo='bar'></p>\n",
                 render("%p{:foo => 'bar', :bar => false, :baz => 'false'}", :format => :html4))
    assert_equal("<p baz='false' foo='bar'></p>\n",
                 render("%p{:foo => 'bar', :bar => false, :baz => 'false'}", :format => :xhtml))
  end

  def test_both_whitespace_nukes_work_together
    assert_equal(<<RESULT, render(<<SOURCE))
<p><q>Foo
  Bar</q></p>
RESULT
%p
  %q><= "Foo\\nBar"
SOURCE
  end

  def test_nil_option
    assert_equal("<p foo='bar'></p>\n", render('%p{:foo => "bar"}', :attr_wrapper => nil))
  end

  # Regression tests

  def test_whitespace_nuke_with_both_newlines
    assert_equal("<p>foo</p>\n", render('%p<= "\nfoo\n"'))
    assert_equal(<<HTML, render(<<HAML))
<p>
  <p>foo</p>
</p>
HTML
%p
  %p<= "\\nfoo\\n"
HAML
  end

  def test_whitespace_nuke_with_tags_and_else
    assert_equal(<<HTML, render(<<HAML))
<a>
  <b>foo</b>
</a>
HTML
%a
  %b<
    - if false
      = "foo"
    - else
      foo
HAML

    assert_equal(<<HTML, render(<<HAML))
<a>
  <b>
    foo
  </b>
</a>
HTML
%a
  %b
    - if false
      = "foo"
    - else
      foo
HAML
  end

  def test_outer_whitespace_nuke_with_empty_script
    assert_equal(<<HTML, render(<<HAML))
<p>
  foo<a></a></p>
HTML
%p
  foo
  = "  "
  %a>
HAML
  end

  def test_both_case_indentation_work_with_deeply_nested_code
    result = <<RESULT
<h2>
  other
</h2>
RESULT
    assert_equal(result, render(<<HAML))
- case 'other'
- when 'test'
  %h2
    hi
- when 'other'
  %h2
    other
HAML
    assert_equal(result, render(<<HAML))
- case 'other'
  - when 'test'
    %h2
      hi
  - when 'other'
    %h2
      other
HAML
  end

  def test_equals_block_with_ugly
    assert_equal("foo\n", render(<<HAML, :ugly => true))
= capture_haml do
  foo
HAML
  end

  def test_plain_equals_with_ugly
    assert_equal("foo\nbar\n", render(<<HAML, :ugly => true))
= "foo"
bar
HAML
  end

  def test_inline_if
    assert_equal(<<HTML, render(<<HAML))
<p>One</p>
<p></p>
<p>Three</p>
HTML
- for name in ["One", "Two", "Three"]
  %p= name unless name == "Two"
HAML
  end

  def test_end_with_method_call
    assert_equal(<<HTML, render(<<HAML))
2|3|4
b-a-r
HTML
= [1, 2, 3].map do |i|
  - i + 1
- end.join("|")
= "bar".gsub(/./) do |s|
  - s + "-"
- end.gsub(/-$/) do |s|
  - ''
HAML
  end

  def test_nested_end_with_method_call
    assert_equal(<<HTML, render(<<HAML))
<p>
  2|3|4
  b-a-r
</p>
HTML
%p
  = [1, 2, 3].map do |i|
    - i + 1
  - end.join("|")
  = "bar".gsub(/./) do |s|
    - s + "-"
  - end.gsub(/-$/) do |s|
    - ''
HAML
  end

  def test_silent_end_with_stuff
    assert_equal(<<HTML, render(<<HAML))
e
d
c
b
a
HTML
- str = "abcde"
- if true
  = str.slice!(-1).chr
- end until str.empty?
HAML

    assert_equal(<<HTML, render(<<HAML))
<p>hi!</p>
HTML
- if true
  %p hi!
- end if "foo".gsub(/f/) do
  - "z"
- end + "bar"
HAML
  end

  def test_multiline_with_colon_after_filter
    assert_equal(<<HTML, render(<<HAML))
Foo
Bar
HTML
:plain
  Foo
= { :a => "Bar",      |
    :b => "Baz" }[:a] |
HAML
    assert_equal(<<HTML, render(<<HAML))

Bar
HTML
:plain
= { :a => "Bar",      |
    :b => "Baz" }[:a] |
HAML
  end

  def test_multiline_in_filter
    assert_equal(<<HTML, render(<<HAML))
Foo |
Bar |
Baz
HTML
:plain
  Foo |
  Bar |
  Baz
HAML
  end

  def test_curly_brace
    assert_equal(<<HTML, render(<<HAML))
Foo { Bar
HTML
== Foo { Bar
HAML
  end

  def test_escape_html
    html = <<HTML
&amp;
&
&amp;
HTML

    assert_equal(html, render(<<HAML, :escape_html => true))
&= "&"
!= "&"
= "&"
HAML

    assert_equal(html, render(<<HAML, :escape_html => true))
&~ "&"
!~ "&"
~ "&"
HAML

    assert_equal(html, render(<<HAML, :escape_html => true))
& \#{"&"}
! \#{"&"}
\#{"&"}
HAML

    assert_equal(html, render(<<HAML, :escape_html => true))
&== \#{"&"}
!== \#{"&"}
== \#{"&"}
HAML

    tag_html = <<HTML
<p>&amp;</p>
<p>&</p>
<p>&amp;</p>
HTML

    assert_equal(tag_html, render(<<HAML, :escape_html => true))
%p&= "&"
%p!= "&"
%p= "&"
HAML

    assert_equal(tag_html, render(<<HAML, :escape_html => true))
%p&~ "&"
%p!~ "&"
%p~ "&"
HAML

    assert_equal(tag_html, render(<<HAML, :escape_html => true))
%p& \#{"&"}
%p! \#{"&"}
%p \#{"&"}
HAML

    assert_equal(tag_html, render(<<HAML, :escape_html => true))
%p&== \#{"&"}
%p!== \#{"&"}
%p== \#{"&"}
HAML
  end

  def test_new_attrs_with_hash
    assert_equal("<a href='#'></a>\n", render('%a(href="#")'))
  end

  def test_javascript_filter_with_dynamic_interp_and_escape_html
    assert_equal(<<HTML, render(<<HAML, :escape_html => true))
<script type='text/javascript'>
  //<![CDATA[
    & < > &
  //]]>
</script>
HTML
:javascript
 & < > \#{"&"}
HAML
  end

  def test_erb_filter_with_multiline_expr
    assert_equal(<<HTML, render(<<HAML))
foobarbaz
HTML
:erb
  <%= "foo" +
      "bar" +
      "baz" %>
HAML
  end

  def test_silent_script_with_hyphen_case
    assert_equal("", render("- 'foo-case-bar-case'"))
  end

  def test_silent_script_with_hyphen_end
    assert_equal("", render("- 'foo-end-bar-end'"))
  end

  def test_silent_script_with_hyphen_end_and_block
    assert_equal(<<HTML, render(<<HAML))
<p>foo-end</p>
<p>bar-end</p>
HTML
- ("foo-end-bar-end".gsub(/\\w+-end/) do |s|
  %p= s
- end; nil)
HAML
  end

  def test_if_without_content_and_else
    assert_equal(<<HTML, render(<<HAML))
foo
HTML
- if false
- else
  foo
HAML

    assert_equal(<<HTML, render(<<HAML))
foo
HTML
- if true
  - if false
  - else
    foo
HAML
  end

  def test_html_attributes_with_hash
    assert_equal("<a href='#' rel='top'>Foo</a>\n",
      render('%a(href="#" rel="top") Foo'))
    assert_equal("<a href='#'>Foo</a>\n",
      render('%a(href="#") #{"Foo"}'))

    assert_equal("<a href='#\"'></a>\n", render('%a(href="#\\"")'))
  end

  def test_filter_with_newline_and_interp
    assert_equal(<<HTML, render(<<HAML))
\\n
HTML
:plain
  \\n\#{""}
HAML
  end

  def test_case_assigned_to_var
    assert_equal(<<HTML, render(<<HAML))
bar
HTML
- var = case 12
- when 1; "foo"
- when 12; "bar"
= var
HAML

    assert_equal(<<HTML, render(<<HAML))
bar
HTML
- var = case 12
- when 1
  - "foo"
- when 12
  - "bar"
= var
HAML

    assert_equal(<<HTML, render(<<HAML))
bar
HTML
- var = case 12
  - when 1
    - "foo"
  - when 12
    - "bar"
= var
HAML
  end

  def test_if_assigned_to_var
    assert_equal(<<HTML, render(<<HAML))
foo
HTML
- var = if false
- else
  - "foo"
= var
HAML

    assert_equal(<<HTML, render(<<HAML))
foo
HTML
- var = if false
- elsif 12
  - "foo"
- elsif 14; "bar"
- else
  - "baz"
= var
HAML

    assert_equal(<<HTML, render(<<HAML))
foo
HTML
- var = if false
  - "bar"
- else
  - "foo"
= var
HAML
  end

  # HTML escaping tests

  def test_ampersand_equals_should_escape
    assert_equal("<p>\n  foo &amp; bar\n</p>\n", render("%p\n  &= 'foo & bar'", :escape_html => false))
  end

  def test_ampersand_equals_inline_should_escape
    assert_equal("<p>foo &amp; bar</p>\n", render("%p&= 'foo & bar'", :escape_html => false))
  end

  def test_ampersand_equals_should_escape_before_preserve
    assert_equal("<textarea>foo&#x000A;bar</textarea>\n", render('%textarea&= "foo\nbar"', :escape_html => false))
  end

  def test_bang_equals_should_not_escape
    assert_equal("<p>\n  foo & bar\n</p>\n", render("%p\n  != 'foo & bar'", :escape_html => true))
  end

  def test_bang_equals_inline_should_not_escape
    assert_equal("<p>foo & bar</p>\n", render("%p!= 'foo & bar'", :escape_html => true))
  end
  
  def test_static_attributes_should_be_escaped
    assert_equal("<img class='atlantis' style='ugly&amp;stupid' />\n",
                 render("%img.atlantis{:style => 'ugly&stupid'}"))
    assert_equal("<div class='atlantis' style='ugly&amp;stupid'>foo</div>\n",
                 render(".atlantis{:style => 'ugly&stupid'} foo"))
    assert_equal("<p class='atlantis' style='ugly&amp;stupid'>foo</p>\n",
                render("%p.atlantis{:style => 'ugly&stupid'}= 'foo'"))
    assert_equal("<p class='atlantis' style='ugly&#x000A;stupid'></p>\n",
                render("%p.atlantis{:style => \"ugly\\nstupid\"}"))
  end

  def test_dynamic_attributes_should_be_escaped
    assert_equal("<img alt='' src='&amp;foo.png' />\n",
                 render("%img{:width => nil, :src => '&foo.png', :alt => String.new}"))
    assert_equal("<p alt='' src='&amp;foo.png'>foo</p>\n",
                 render("%p{:width => nil, :src => '&foo.png', :alt => String.new} foo"))
    assert_equal("<div alt='' src='&amp;foo.png'>foo</div>\n",
                 render("%div{:width => nil, :src => '&foo.png', :alt => String.new}= 'foo'"))
    assert_equal("<img alt='' src='foo&#x000A;.png' />\n",
                 render("%img{:width => nil, :src => \"foo\\n.png\", :alt => String.new}"))
  end

  def test_string_double_equals_should_be_esaped
    assert_equal("<p>4&&lt;</p>\n", render("%p== \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&<</p>\n", render("%p== \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_escaped_inline_string_double_equals
    assert_equal("<p>4&&lt;</p>\n", render("%p&== \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&&lt;</p>\n", render("%p&== \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_unescaped_inline_string_double_equals
    assert_equal("<p>4&<</p>\n", render("%p!== \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&<</p>\n", render("%p!== \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_escaped_string_double_equals
    assert_equal("<p>\n  4&&lt;\n</p>\n", render("%p\n  &== \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>\n  4&&lt;\n</p>\n", render("%p\n  &== \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_unescaped_string_double_equals
    assert_equal("<p>\n  4&<\n</p>\n", render("%p\n  !== \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>\n  4&<\n</p>\n", render("%p\n  !== \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_string_interpolation_should_be_esaped
    assert_equal("<p>4&&lt;</p>\n", render("%p \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&<</p>\n", render("%p \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_escaped_inline_string_interpolation
    assert_equal("<p>4&&lt;</p>\n", render("%p& \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&&lt;</p>\n", render("%p& \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_unescaped_inline_string_interpolation
    assert_equal("<p>4&<</p>\n", render("%p! \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>4&<</p>\n", render("%p! \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_escaped_string_interpolation
    assert_equal("<p>\n  4&&lt;\n</p>\n", render("%p\n  & \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>\n  4&&lt;\n</p>\n", render("%p\n  & \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_unescaped_string_interpolation
    assert_equal("<p>\n  4&<\n</p>\n", render("%p\n  ! \#{2+2}&\#{'<'}", :escape_html => true))
    assert_equal("<p>\n  4&<\n</p>\n", render("%p\n  ! \#{2+2}&\#{'<'}", :escape_html => false))
  end

  def test_scripts_should_respect_escape_html_option
    assert_equal("<p>\n  foo &amp; bar\n</p>\n", render("%p\n  = 'foo & bar'", :escape_html => true))
    assert_equal("<p>\n  foo & bar\n</p>\n", render("%p\n  = 'foo & bar'", :escape_html => false))
  end

  def test_inline_scripts_should_respect_escape_html_option
    assert_equal("<p>foo &amp; bar</p>\n", render("%p= 'foo & bar'", :escape_html => true))
    assert_equal("<p>foo & bar</p>\n", render("%p= 'foo & bar'", :escape_html => false))
  end

  def test_script_ending_in_comment_should_render_when_html_is_escaped
    assert_equal("foo&amp;bar\n", render("= 'foo&bar' #comment", :escape_html => true))
  end

  def test_script_with_if_shouldnt_output
    assert_equal(<<HTML, render(<<HAML))
<p>foo</p>
<p></p>
HTML
%p= "foo"
%p= "bar" if false
HAML
  end

  # Options tests

  def test_filename_and_line
    begin
      render("\n\n = abc", :filename => 'test', :line => 2)
    rescue Exception => e
      assert_kind_of Haml::SyntaxError, e
      assert_match(/test:4/, e.backtrace.first)
    end

    begin
      render("\n\n= 123\n\n= nil[]", :filename => 'test', :line => 2)
    rescue Exception => e
      assert_kind_of NoMethodError, e
      assert_match(/test:6/, e.backtrace.first)
    end
  end

  def test_stop_eval
    assert_equal("", render("= 'Hello'", :suppress_eval => true))
    assert_equal("", render("- haml_concat 'foo'", :suppress_eval => true))
    assert_equal("<div id='foo' yes='no' />\n", render("#foo{:yes => 'no'}/", :suppress_eval => true))
    assert_equal("<div id='foo' />\n", render("#foo{:yes => 'no', :call => a_function() }/", :suppress_eval => true))
    assert_equal("<div />\n", render("%div[1]/", :suppress_eval => true))
    assert_equal("", render(":ruby\n  Kernel.puts 'hello'", :suppress_eval => true))
  end

  def test_doctypes
    assert_equal('<!DOCTYPE html>',
      render('!!!', :format => :html5).strip)
    assert_equal('<!DOCTYPE html>', render('!!! 5').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">',
      render('!!! strict').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Frameset//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd">',
      render('!!! frameset').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//WAPFORUM//DTD XHTML Mobile 1.2//EN" "http://www.openmobilealliance.org/tech/DTD/xhtml-mobile12.dtd">',
      render('!!! mobile').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML Basic 1.1//EN" "http://www.w3.org/TR/xhtml-basic/xhtml-basic11.dtd">',
      render('!!! basic').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">',
      render('!!! transitional').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">',
      render('!!!').strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">',
      render('!!! strict', :format => :html4).strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN" "http://www.w3.org/TR/html4/frameset.dtd">',
      render('!!! frameset', :format => :html4).strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">',
      render('!!! transitional', :format => :html4).strip)
    assert_equal('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">',
      render('!!!', :format => :html4).strip)
  end

  def test_attr_wrapper
    assert_equal("<p strange=*attrs*></p>\n", render("%p{ :strange => 'attrs'}", :attr_wrapper => '*'))
    assert_equal("<p escaped='quo\"te'></p>\n", render("%p{ :escaped => 'quo\"te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"quo'te\"></p>\n", render("%p{ :escaped => 'quo\\'te'}", :attr_wrapper => '"'))
    assert_equal("<p escaped=\"q'uo&quot;te\"></p>\n", render("%p{ :escaped => 'q\\'uo\"te'}", :attr_wrapper => '"'))
    assert_equal("<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n", render("!!! XML", :attr_wrapper => '"'))
  end

  def test_autoclose_option
    assert_equal("<flaz foo='bar' />\n", render("%flaz{:foo => 'bar'}", :autoclose => ["flaz"]))
    assert_equal(<<HTML, render(<<HAML, :autoclose => [/^flaz/]))
<flaz />
<flaznicate />
<flan></flan>
HTML
%flaz
%flaznicate
%flan
HAML
  end

  def test_attrs_parsed_correctly
    assert_equal("<p boom=>biddly='bar =&gt; baz'></p>\n", render("%p{'boom=>biddly' => 'bar => baz'}"))
    assert_equal("<p foo,bar='baz, qux'></p>\n", render("%p{'foo,bar' => 'baz, qux'}"))
    assert_equal("<p escaped='quo&#x000A;te'></p>\n", render("%p{ :escaped => \"quo\\nte\"}"))
    assert_equal("<p escaped='quo4te'></p>\n", render("%p{ :escaped => \"quo\#{2 + 2}te\"}"))
  end

  def test_correct_parsing_with_brackets
    assert_equal("<p class='foo'>{tada} foo</p>\n", render("%p{:class => 'foo'} {tada} foo"))
    assert_equal("<p class='foo'>deep {nested { things }}</p>\n", render("%p{:class => 'foo'} deep {nested { things }}"))
    assert_equal("<p class='bar foo'>{a { d</p>\n", render("%p{{:class => 'foo'}, :class => 'bar'} {a { d"))
    assert_equal("<p foo='bar'>a}</p>\n", render("%p{:foo => 'bar'} a}"))
    
    foo = []
    foo[0] = Struct.new('Foo', :id).new
    assert_equal("<p class='struct_foo' id='struct_foo_new'>New User]</p>\n",
                 render("%p[foo[0]] New User]", :locals => {:foo => foo}))
    assert_equal("<p class='prefix_struct_foo' id='prefix_struct_foo_new'>New User]</p>\n",
                 render("%p[foo[0], :prefix] New User]", :locals => {:foo => foo}))

    foo[0].id = 1
    assert_equal("<p class='struct_foo' id='struct_foo_1'>New User]</p>\n",
                 render("%p[foo[0]] New User]", :locals => {:foo => foo}))
    assert_equal("<p class='prefix_struct_foo' id='prefix_struct_foo_1'>New User]</p>\n",
                 render("%p[foo[0], :prefix] New User]", :locals => {:foo => foo}))
  end
  
  def test_empty_attrs
    assert_equal("<p attr=''>empty</p>\n", render("%p{ :attr => '' } empty"))
    assert_equal("<p attr=''>empty</p>\n", render("%p{ :attr => x } empty", :locals => {:x => ''}))
  end
  
  def test_nil_attrs
    assert_equal("<p>nil</p>\n", render("%p{ :attr => nil } nil"))
    assert_equal("<p>nil</p>\n", render("%p{ :attr => x } nil", :locals => {:x => nil}))
  end

  def test_nil_id_with_syntactic_id
    assert_equal("<p id='foo'>nil</p>\n", render("%p#foo{:id => nil} nil"))
    assert_equal("<p id='foo_bar'>nil</p>\n", render("%p#foo{{:id => 'bar'}, :id => nil} nil"))
    assert_equal("<p id='foo_bar'>nil</p>\n", render("%p#foo{{:id => nil}, :id => 'bar'} nil"))
  end

  def test_nil_class_with_syntactic_class
    assert_equal("<p class='foo'>nil</p>\n", render("%p.foo{:class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.bar.foo{:class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.foo{{:class => 'bar'}, :class => nil} nil"))
    assert_equal("<p class='bar foo'>nil</p>\n", render("%p.foo{{:class => nil}, :class => 'bar'} nil"))
  end

  def test_locals
    assert_equal("<p>Paragraph!</p>\n", render("%p= text", :locals => { :text => "Paragraph!" }))
  end

  def test_dynamic_attrs_shouldnt_register_as_literal_values
    assert_equal("<p a='b2c'></p>\n", render('%p{:a => "b#{1 + 1}c"}'))
    assert_equal("<p a='b2c'></p>\n", render("%p{:a => 'b' + (1 + 1).to_s + 'c'}"))
  end

  def test_dynamic_attrs_with_self_closed_tag
    assert_equal("<a b='2' />\nc\n", render("%a{'b' => 1 + 1}/\n= 'c'\n"))
  end

  EXCEPTION_MAP.each do |key, value|
    define_method("test_exception (#{key.inspect})") do
      begin
        render(key, :filename => __FILE__)
      rescue Exception => err
        value = [value] unless value.is_a?(Array)
        expected_message, line_no = value
        line_no ||= key.split("\n").length

        if expected_message == :compile
          if Haml::Util.ruby1_8?
            assert_match(/^compile error\n/, err.message, "Line: #{key}")
          else
            assert_match(/^#{Regexp.quote __FILE__}:#{line_no}: syntax error,/, err.message, "Line: #{key}")
          end
        else
          assert_equal(expected_message, err.message, "Line: #{key}")
        end

        if Haml::Util.ruby1_8?
          assert_match(/^#{Regexp.escape(__FILE__)}:#{line_no}/, err.backtrace[0], "Line: #{key}")
        end
      else
        assert(false, "Exception not raised for\n#{key}")
      end
    end
  end

  def test_exception_line
    render("a\nb\n!!!\n  c\nd")
  rescue Haml::SyntaxError => e
    assert_equal("(test_exception_line):4", e.backtrace[0])
  else
    assert(false, '"a\nb\n!!!\n  c\nd" doesn\'t produce an exception')
  end

  def test_exception
    render("%p\n  hi\n  %a= undefined\n= 12")
  rescue Exception => e
    assert_match("(test_exception):3", e.backtrace[0])
  else
    # Test failed... should have raised an exception
    assert(false)
  end

  def test_compile_error
    render("a\nb\n- fee)\nc")
  rescue Exception => e
    assert_match(/\(test_compile_error\):3: syntax error/i, e.message)
  else
    assert(false,
           '"a\nb\n- fee)\nc" doesn\'t produce an exception!')
  end

  def test_unbalanced_brackets
    render('foo #{1 + 5} foo #{6 + 7 bar #{8 + 9}')
  rescue Haml::SyntaxError => e
    assert_equal("Unbalanced brackets.", e.message)
  end

  def test_balanced_conditional_comments
    assert_equal("<!--[if !(IE 6)|(IE 7)]> Bracket: ] <![endif]-->\n",
                 render("/[if !(IE 6)|(IE 7)] Bracket: ]"))
  end

  def test_empty_filter
    assert_equal(<<END, render(':javascript'))
<script type='text/javascript'>
  //<![CDATA[
    
  //]]>
</script>
END
  end

  def test_ugly_filter
    assert_equal(<<END, render(":sass\n  #foo\n    bar: baz", :ugly => true))
#foo {
  bar: baz; }
END
  end

  def test_css_filter
    assert_equal(<<CSS, render(<<SASS))
<style type='text/css'>
  /*<![CDATA[*/
    #foo {
      bar: baz; }
  /*]]>*/
</style>
CSS
:css
  #foo {
    bar: baz; }
SASS
  end

  def test_local_assigns_dont_modify_class
    assert_equal("bar\n", render("= foo", :locals => {:foo => 'bar'}))
    assert_equal(nil, defined?(foo))
  end

  def test_object_ref_with_nil_id
    user = User.new
    assert_equal("<p class='struct_user' id='struct_user_new'>New User</p>\n",
                 render("%p[user] New User", :locals => {:user => user}))
  end

  def test_object_ref_before_attrs
    user = User.new 42
    assert_equal("<p class='struct_user' id='struct_user_42' style='width: 100px;'>New User</p>\n",
                 render("%p[user]{:style => 'width: 100px;'} New User", :locals => {:user => user}))
  end

  def test_object_ref_with_custom_haml_class
    custom = CustomHamlClass.new 42
    assert_equal("<p class='my_thing' id='my_thing_42' style='width: 100px;'>My Thing</p>\n",
                 render("%p[custom]{:style => 'width: 100px;'} My Thing", :locals => {:custom => custom}))
  end

  def test_non_literal_attributes
    assert_equal("<p a1='foo' a2='bar' a3='baz' />\n",
                 render("%p{a2, a1, :a3 => 'baz'}/",
                        :locals => {:a1 => {:a1 => 'foo'}, :a2 => {:a2 => 'bar'}}))
  end

  def test_render_should_accept_a_binding_as_scope
    string = "This is a string!"
    string.instance_variable_set("@var", "Instance variable")
    b = string.instance_eval do
      var = "Local variable"
      binding
    end

    assert_equal("<p>THIS IS A STRING!</p>\n<p>Instance variable</p>\n<p>Local variable</p>\n",
                 render("%p= upcase\n%p= @var\n%p= var", :scope => b))
  end

  def test_yield_should_work_with_binding
    assert_equal("12\nFOO\n", render("= yield\n= upcase", :scope => "foo".instance_eval{binding}) { 12 })
  end

  def test_yield_should_work_with_def_method
    s = "foo"
    engine("= yield\n= upcase").def_method(s, :render)
    assert_equal("12\nFOO\n", s.render { 12 })
  end

  def test_def_method_with_module
    engine("= yield\n= upcase").def_method(String, :render_haml)
    assert_equal("12\nFOO\n", "foo".render_haml { 12 })
  end

  def test_def_method_locals
    obj = Object.new
    engine("%p= foo\n.bar{:baz => baz}= boom").def_method(obj, :render, :foo, :baz, :boom)
    assert_equal("<p>1</p>\n<div baz='2' class='bar'>3</div>\n", obj.render(:foo => 1, :baz => 2, :boom => 3))
  end

  def test_render_proc_locals
    proc = engine("%p= foo\n.bar{:baz => baz}= boom").render_proc(Object.new, :foo, :baz, :boom)
    assert_equal("<p>1</p>\n<div baz='2' class='bar'>3</div>\n", proc[:foo => 1, :baz => 2, :boom => 3])
  end

  def test_render_proc_with_binding
    assert_equal("FOO\n", engine("= upcase").render_proc("foo".instance_eval{binding}).call)
  end

  def test_haml_buffer_gets_reset_even_with_exception
    scope = Object.new
    render("- raise Haml::Error", :scope => scope)
    assert(false, "Expected exception")
  rescue Exception
    assert_nil(scope.send(:haml_buffer))
  end

  def test_def_method_haml_buffer_gets_reset_even_with_exception
    scope = Object.new
    engine("- raise Haml::Error").def_method(scope, :render)
    scope.render
    assert(false, "Expected exception")
  rescue Exception
    assert_nil(scope.send(:haml_buffer))
  end

  def test_render_proc_haml_buffer_gets_reset_even_with_exception
    scope = Object.new
    proc = engine("- raise Haml::Error").render_proc(scope)
    proc.call
    assert(false, "Expected exception")
  rescue Exception
    assert_nil(scope.send(:haml_buffer))
  end

  def test_ugly_true
    assert_equal("<div id='outer'>\n<div id='inner'>\n<p>hello world</p>\n</div>\n</div>\n",
                 render("#outer\n  #inner\n    %p hello world", :ugly => true))

    assert_equal("<p>#{'s' * 75}</p>\n",
                 render("%p #{'s' * 75}", :ugly => true))

    assert_equal("<p>#{'s' * 75}</p>\n",
                 render("%p= 's' * 75", :ugly => true))
  end

  def test_auto_preserve_unless_ugly
    assert_equal("<pre>foo&#x000A;bar</pre>\n", render('%pre="foo\nbar"'))
    assert_equal("<pre>foo\nbar</pre>\n", render("%pre\n  foo\n  bar"))
    assert_equal("<pre>foo\nbar</pre>\n", render('%pre="foo\nbar"', :ugly => true))
    assert_equal("<pre>foo\nbar</pre>\n", render("%pre\n  foo\n  bar", :ugly => true))
  end

  def test_xhtml_output_option
    assert_equal "<p>\n  <br />\n</p>\n", render("%p\n  %br", :format => :xhtml)
    assert_equal "<a />\n", render("%a/", :format => :xhtml)
  end

  def test_arbitrary_output_option
    assert_raise_message(Haml::Error, "Invalid output format :html1") do
      engine("%br", :format => :html1)
    end
  end

  def test_static_hashes
    assert_equal("<a b='a =&gt; b'></a>\n", render("%a{:b => 'a => b'}", :suppress_eval => true))
    assert_equal("<a b='a, b'></a>\n", render("%a{:b => 'a, b'}", :suppress_eval => true))
    assert_equal("<a b='a\tb'></a>\n", render('%a{:b => "a\tb"}', :suppress_eval => true))
    assert_equal("<a b='a\#{foo}b'></a>\n", render('%a{:b => "a\\#{foo}b"}', :suppress_eval => true))
  end

  def test_dynamic_hashes_with_suppress_eval
    assert_equal("<a></a>\n", render('%a{:b => "a #{1 + 1} b", :c => "d"}', :suppress_eval => true))
  end

  def test_utf8_attrs
    assert_equal("<a href='héllo'></a>\n", render("%a{:href => 'héllo'}"))
    assert_equal("<a href='héllo'></a>\n", render("%a(href='héllo')"))
  end

  # HTML 4.0

  def test_html_has_no_self_closing_tags
    assert_equal "<p>\n  <br>\n</p>\n", render("%p\n  %br", :format => :html4)
    assert_equal "<br>\n", render("%br/", :format => :html4)
  end

  def test_html_renders_empty_node_with_closing_tag
    assert_equal "<div class='foo'></div>\n", render(".foo", :format => :html4)
  end

  def test_html_doesnt_add_slash_to_self_closing_tags
    assert_equal "<a>\n", render("%a/", :format => :html4)
    assert_equal "<a foo='2'>\n", render("%a{:foo => 1 + 1}/", :format => :html4)
    assert_equal "<meta>\n", render("%meta", :format => :html4)
    assert_equal "<meta foo='2'>\n", render("%meta{:foo => 1 + 1}", :format => :html4)
  end

  def test_html_ignores_xml_prolog_declaration
    assert_equal "", render('!!! XML', :format => :html4)
  end

  def test_html_has_different_doctype
    assert_equal %{<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">\n},
    render('!!!', :format => :html4)
  end

  # because anything before the doctype triggers quirks mode in IE
  def test_xml_prolog_and_doctype_dont_result_in_a_leading_whitespace_in_html
    assert_no_match(/^\s+/, render("!!! xml\n!!!", :format => :html4))
  end

  # HTML5
  def test_html5_doctype
    assert_equal %{<!DOCTYPE html>\n}, render('!!!', :format => :html5)
  end

  # HTML5 custom data attributes
  def test_html5_data_attributes
    assert_equal("<div data-author_id='123' data-biz='baz' data-foo='bar'></div>\n",
      render("%div{:data => {:author_id => 123, :foo => 'bar', :biz => 'baz'}}"))

    assert_equal("<div data-one_plus_one='2'></div>\n",
      render("%div{:data => {:one_plus_one => 1+1}}"))

    assert_equal("<div data-foo='Here&apos;s a \"quoteful\" string.'></div>\n",
      render(%{%div{:data => {:foo => %{Here's a "quoteful" string.}}}})) #'
  end

  def test_html5_data_attributes_with_multiple_defs
    # Should always use the more-explicit attribute
    assert_equal("<div data-foo='second'></div>\n",
      render("%div{:data => {:foo => 'first'}, 'data-foo' => 'second'}"))
    assert_equal("<div data-foo='first'></div>\n",
      render("%div{'data-foo' => 'first', :data => {:foo => 'second'}}"))
  end

  def test_html5_data_attributes_with_attr_method
    Haml::Helpers.module_eval do
      def data_hash
        {:data => {:foo => "bar", :baz => "bang"}}
      end

      def data_val
        {:data => "dat"}
      end
    end

    assert_equal("<div data-baz='bang' data-brat='wurst' data-foo='blip'></div>\n",
      render("%div{data_hash, :data => {:foo => 'blip', :brat => 'wurst'}}"))
    assert_equal("<div data-baz='bang' data-foo='blip'></div>\n",
      render("%div{data_hash, 'data-foo' => 'blip'}"))
    assert_equal("<div data-baz='bang' data-foo='bar' data='dat'></div>\n",
      render("%div{data_hash, :data => 'dat'}"))
    assert_equal("<div data-brat='wurst' data-foo='blip' data='dat'></div>\n",
      render("%div{data_val, :data => {:foo => 'blip', :brat => 'wurst'}}"))
  end

  # New attributes

  def test_basic_new_attributes
    assert_equal("<a>bar</a>\n", render("%a() bar"))
    assert_equal("<a href='foo'>bar</a>\n", render("%a(href='foo') bar"))
    assert_equal("<a b='c' c='d' d='e'>baz</a>\n", render(%q{%a(b="c" c='d' d="e") baz}))
  end

  def test_new_attribute_ids
    assert_equal("<div id='foo_bar'></div>\n", render("#foo(id='bar')"))
    assert_equal("<div id='foo_bar_baz'></div>\n", render("#foo{:id => 'bar'}(id='baz')"))
    assert_equal("<div id='foo_baz_bar'></div>\n", render("#foo(id='baz'){:id => 'bar'}"))
    foo = User.new(42)
    assert_equal("<div class='struct_user' id='foo_baz_bar_struct_user_42'></div>\n",
      render("#foo(id='baz'){:id => 'bar'}[foo]", :locals => {:foo => foo}))
    assert_equal("<div class='struct_user' id='foo_baz_bar_struct_user_42'></div>\n",
      render("#foo(id='baz')[foo]{:id => 'bar'}", :locals => {:foo => foo}))
    assert_equal("<div class='struct_user' id='foo_baz_bar_struct_user_42'></div>\n",
      render("#foo[foo](id='baz'){:id => 'bar'}", :locals => {:foo => foo}))
    assert_equal("<div class='struct_user' id='foo_bar_baz_struct_user_42'></div>\n",
      render("#foo[foo]{:id => 'bar'}(id='baz')", :locals => {:foo => foo}))
  end

  def test_new_attribute_classes
    assert_equal("<div class='bar foo'></div>\n", render(".foo(class='bar')"))
    assert_equal("<div class='bar baz foo'></div>\n", render(".foo{:class => 'bar'}(class='baz')"))
    assert_equal("<div class='bar baz foo'></div>\n", render(".foo(class='baz'){:class => 'bar'}"))
    foo = User.new(42)
    assert_equal("<div class='bar baz foo struct_user' id='struct_user_42'></div>\n",
      render(".foo(class='baz'){:class => 'bar'}[foo]", :locals => {:foo => foo}))
    assert_equal("<div class='bar baz foo struct_user' id='struct_user_42'></div>\n",
      render(".foo[foo](class='baz'){:class => 'bar'}", :locals => {:foo => foo}))
    assert_equal("<div class='bar baz foo struct_user' id='struct_user_42'></div>\n",
      render(".foo[foo]{:class => 'bar'}(class='baz')", :locals => {:foo => foo}))
  end

  def test_dynamic_new_attributes
    assert_equal("<a href='12'>bar</a>\n", render("%a(href=foo) bar", :locals => {:foo => 12}))
    assert_equal("<a b='12' c='13' d='14'>bar</a>\n", render("%a(b=b c='13' d=d) bar", :locals => {:b => 12, :d => 14}))
  end

  def test_new_attribute_interpolation
    assert_equal("<a href='12'>bar</a>\n", render('%a(href="1#{1 + 1}") bar'))
    assert_equal("<a href='2: 2, 3: 3'>bar</a>\n", render(%q{%a(href='2: #{1 + 1}, 3: #{foo}') bar}, :locals => {:foo => 3}))
    assert_equal(%Q{<a href='1\#{1 + 1}'>bar</a>\n}, render('%a(href="1\#{1 + 1}") bar'))
  end

  def test_truthy_new_attributes
    assert_equal("<a href='href'>bar</a>\n", render("%a(href) bar"))
    assert_equal("<a bar='baz' href>bar</a>\n", render("%a(href bar='baz') bar", :format => :html5))
    assert_equal("<a href='href'>bar</a>\n", render("%a(href=true) bar"))
    assert_equal("<a>bar</a>\n", render("%a(href=false) bar"))
  end

  def test_new_attribute_parsing
    assert_equal("<a a2='b2'>bar</a>\n", render("%a(a2=b2) bar", :locals => {:b2 => 'b2'}))
    assert_equal(%Q{<a a='foo"bar'>bar</a>\n}, render(%q{%a(a="#{'foo"bar'}") bar})) #'
    assert_equal(%Q{<a a="foo'bar">bar</a>\n}, render(%q{%a(a="#{"foo'bar"}") bar})) #'
    assert_equal(%Q{<a a='foo"bar'>bar</a>\n}, render(%q{%a(a='foo"bar') bar}))
    assert_equal(%Q{<a a="foo'bar">bar</a>\n}, render(%q{%a(a="foo'bar") bar}))
    assert_equal("<a a:b='foo'>bar</a>\n", render("%a(a:b='foo') bar"))
    assert_equal("<a a='foo' b='bar'>bar</a>\n", render("%a(a = 'foo' b = 'bar') bar"))
    assert_equal("<a a='foo' b='bar'>bar</a>\n", render("%a(a = foo b = bar) bar", :locals => {:foo => 'foo', :bar => 'bar'}))
    assert_equal("<a a='foo'>(b='bar')</a>\n", render("%a(a='foo')(b='bar')"))
    assert_equal("<a a='foo)bar'>baz</a>\n", render("%a(a='foo)bar') baz"))
    assert_equal("<a a='foo'>baz</a>\n", render("%a( a = 'foo' ) baz"))
  end

  def test_new_attribute_escaping
    assert_equal(%Q{<a a='foo " bar'>bar</a>\n}, render(%q{%a(a="foo \" bar") bar}))
    assert_equal(%Q{<a a='foo \\" bar'>bar</a>\n}, render(%q{%a(a="foo \\\\\" bar") bar}))

    assert_equal(%Q{<a a="foo ' bar">bar</a>\n}, render(%q{%a(a='foo \' bar') bar}))
    assert_equal(%Q{<a a="foo \\' bar">bar</a>\n}, render(%q{%a(a='foo \\\\\' bar') bar}))

    assert_equal(%Q{<a a='foo \\ bar'>bar</a>\n}, render(%q{%a(a="foo \\\\ bar") bar}))
    assert_equal(%Q{<a a='foo \#{1 + 1} bar'>bar</a>\n}, render(%q{%a(a="foo \#{1 + 1} bar") bar}))
  end

  def test_multiline_new_attribute
    assert_equal("<a a='b' c='d'>bar</a>\n", render("%a(a='b'\n  c='d') bar"))
    assert_equal("<a a='b' b='c' c='d' d='e' e='f' f='j'>bar</a>\n",
      render("%a(a='b' b='c'\n  c='d' d=e\n  e='f' f='j') bar", :locals => {:e => 'e'}))
  end

  def test_new_and_old_attributes
    assert_equal("<a a='b' c='d'>bar</a>\n", render("%a(a='b'){:c => 'd'} bar"))
    assert_equal("<a a='b' c='d'>bar</a>\n", render("%a{:c => 'd'}(a='b') bar"))
    assert_equal("<a a='b' c='d'>bar</a>\n", render("%a(c='d'){:a => 'b'} bar"))
    assert_equal("<a a='b' c='d'>bar</a>\n", render("%a{:a => 'b'}(c='d') bar"))

    assert_equal("<a a='d'>bar</a>\n", render("%a{:a => 'b'}(a='d') bar"))
    assert_equal("<a a='b'>bar</a>\n", render("%a(a='d'){:a => 'b'} bar"))

    assert_equal("<a a='b' b='c' c='d' d='e'>bar</a>\n",
      render("%a{:a => 'b',\n:b => 'c'}(c='d'\nd='e') bar"))
  end

  # Ruby Multiline

  def test_silent_ruby_multiline
    assert_equal(<<HTML, render(<<HAML))
bar, baz, bang
<p>foo</p>
HTML
- foo = ["bar",
         "baz",
         "bang"]
= foo.join(", ")
%p foo
HAML
  end

  def test_loud_ruby_multiline
    assert_equal(<<HTML, render(<<HAML))
bar, baz, bang
<p>foo</p>
<p>bar</p>
HTML
= ["bar",
   "baz",
   "bang"].join(", ")
%p foo
%p bar
HAML
  end

  def test_escaped_loud_ruby_multiline
    assert_equal(<<HTML, render(<<HAML))
bar&lt;, baz, bang
<p>foo</p>
<p>bar</p>
HTML
&= ["bar<",
    "baz",
    "bang"].join(", ")
%p foo
%p bar
HAML
  end

  def test_unescaped_loud_ruby_multiline
    assert_equal(<<HTML, render(<<HAML, :escape_html => true))
bar<, baz, bang
<p>foo</p>
<p>bar</p>
HTML
!= ["bar<",
    "baz",
    "bang"].join(", ")
%p foo
%p bar
HAML
  end

  def test_flattened_loud_ruby_multiline
    assert_equal(<<HTML, render(<<HAML))
<pre>bar&#x000A;baz&#x000A;bang</pre>
<p>foo</p>
<p>bar</p>
HTML
~ "<pre>" + ["bar",
             "baz",
             "bang"].join("\\n") + "</pre>"
%p foo
%p bar
HAML
  end

  def test_loud_ruby_multiline_with_block
    assert_equal(<<HTML, render(<<HAML))
farfazfang
<p>foo</p>
<p>bar</p>
HTML
= ["bar",
   "baz",
   "bang"].map do |str|
  - str.gsub("ba",
             "fa")
%p foo
%p bar
HAML
  end

  def test_silent_ruby_multiline_with_block
    assert_equal(<<HTML, render(<<HAML))
far
faz
fang
<p>foo</p>
<p>bar</p>
HTML
- ["bar",
   "baz",
   "bang"].map do |str|
  = str.gsub("ba",
             "fa")
%p foo
%p bar
HAML
  end

  def test_ruby_multiline_in_tag
    assert_equal(<<HTML, render(<<HAML))
<p>foo, bar, baz</p>
<p>foo</p>
<p>bar</p>
HTML
%p= ["foo",
     "bar",
     "baz"].join(", ")
%p foo
%p bar
HAML
  end

  def test_escaped_ruby_multiline_in_tag
    assert_equal(<<HTML, render(<<HAML))
<p>foo&lt;, bar, baz</p>
<p>foo</p>
<p>bar</p>
HTML
%p&= ["foo<",
      "bar",
      "baz"].join(", ")
%p foo
%p bar
HAML
  end

  def test_unescaped_ruby_multiline_in_tag
    assert_equal(<<HTML, render(<<HAML, :escape_html => true))
<p>foo<, bar, baz</p>
<p>foo</p>
<p>bar</p>
HTML
%p!= ["foo<",
      "bar",
      "baz"].join(", ")
%p foo
%p bar
HAML
  end

  def test_ruby_multiline_with_normal_multiline
    assert_equal(<<HTML, render(<<HAML))
foobarbar, baz, bang
<p>foo</p>
<p>bar</p>
HTML
= "foo" + |
  "bar" + |
  ["bar", |
   "baz",
   "bang"].join(", ")
%p foo
%p bar
HAML
  end

  def test_ruby_multiline_after_filter
    assert_equal(<<HTML, render(<<HAML))
foo
bar
bar, baz, bang
<p>foo</p>
<p>bar</p>
HTML
:plain
  foo
  bar
= ["bar",
   "baz",
   "bang"].join(", ")
%p foo
%p bar
HAML
  end

  # Encodings

  def test_utf_8_bom
    assert_equal <<HTML, render(<<HAML)
<div class='foo'>
  <p>baz</p>
</div>
HTML
\xEF\xBB\xBF.foo
  %p baz
HAML
  end

  unless Haml::Util.ruby1_8?
    def test_default_encoding
      assert_equal(Encoding.find("utf-8"), render(<<HAML.encode("us-ascii")).encoding)
%p bar
%p foo
HAML
    end

    def test_convert_template_render
      assert_equal(<<HTML, render(<<HAML.encode("iso-8859-1"), :encoding => "utf-8"))
<p>bâr</p>
<p>föö</p>
HTML
%p bâr
%p föö
HAML
    end

    def test_fake_ascii_encoding
      assert_equal(<<HTML.force_encoding("ascii-8bit"), render(<<HAML, :encoding => "ascii-8bit"))
<p>bâr</p>
<p>föö</p>
HTML
%p bâr
%p föö
HAML
    end

    def test_convert_template_render_proc
      assert_converts_template_properly {|e| e.render_proc.call}
    end

    def test_convert_template_render
      assert_converts_template_properly {|e| e.render}
    end

    def test_convert_template_def_method
      assert_converts_template_properly do |e|
        o = Object.new
        e.def_method(o, :render)
        o.render
      end
    end

    def test_encoding_error
      render("foo\nbar\nb\xFEaz".force_encoding("utf-8"))
      assert(false, "Expected exception")
    rescue Haml::Error => e
      assert_equal(3, e.line)
      assert_equal('Invalid UTF-8 character "\xFE"', e.message)
    end

    def test_ascii_incompatible_encoding_error
      template = "foo\nbar\nb_z".encode("utf-16le")
      template[9] = "\xFE".force_encoding("utf-16le")
      render(template)
      assert(false, "Expected exception")
    rescue Haml::Error => e
      assert_equal(3, e.line)
      assert_equal('Invalid UTF-16LE character "\xFE"', e.message)
    end

    def test_same_coding_comment_as_encoding
      assert_renders_encoded(<<HTML, <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# coding: utf-8
%p bâr
%p föö
HAML
    end

    def test_different_coding_comment_than_encoding
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# coding: ibm866
%p bâr
%p föö
HAML
    end

    def test_different_coding_than_system
      assert_renders_encoded(<<HTML.encode("IBM866"), <<HAML.encode("IBM866"))
<p>тАЬ</p>
HTML
%p тАЬ
HAML
    end

    def test_case_insensitive_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# CodINg: IbM866
%p bâr
%p föö
HAML
    end

    def test_whitespace_insensitive_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-#coding:ibm866
%p bâr
%p föö
HAML
    end

    def test_equals_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# CodINg= ibm866
%p bâr
%p föö
HAML
    end

    def test_prefixed_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# foo BAR FAOJcoding: ibm866
%p bâr
%p föö
HAML
    end

    def test_suffixed_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# coding: ibm866 ASFJ (&(&#!$
%p bâr
%p föö
HAML
    end

    def test_emacs_prefixed_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- coding: ibm866
%p bâr
%p föö
HAML
    end

    def test_emacs_suffixed_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# coding: ibm866 -*- coding: blah
%p bâr
%p föö
HAML
    end

    def test_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- coding: ibm866 -*-
%p bâr
%p föö
HAML
    end

    def test_emacs_encoding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- encoding: ibm866 -*-
%p bâr
%p föö
HAML
    end

    def test_quoted_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- coding: "ibm866" -*-
%p bâr
%p föö
HAML
    end

    def test_whitespace_insensitive_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-#-*-coding:ibm866-*-
%p bâr
%p föö
HAML
    end

    def test_whitespace_insensitive_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-#-*-coding:ibm866-*-
%p bâr
%p föö
HAML
    end

    def test_one_of_several_emacs_comments
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- foo: bar; coding: ibm866; baz: bang -*-
%p bâr
%p föö
HAML
    end

    def test_prefixed_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# foo bar coding: baz -*- coding: ibm866 -*-
%p bâr
%p föö
HAML
    end

    def test_suffixed_emacs_coding_comment
      assert_renders_encoded(<<HTML.force_encoding("IBM866"), <<HAML)
<p>bâr</p>
<p>föö</p>
HTML
-# -*- coding: ibm866 -*- foo bar coding: baz
%p bâr
%p föö
HAML
    end

  end

  private

  def assert_converts_template_properly
    engine = Haml::Engine.new(<<HAML.encode("iso-8859-1"), :encoding => "utf-8")
%p bâr
%p föö
HAML
    assert_equal(<<HTML, yield(engine))
<p>bâr</p>
<p>föö</p>
HTML
  end

  def assert_renders_encoded(html, haml)
    result = render(haml)
    assert_equal html.encoding, result.encoding
    assert_equal html, result
  end
end
