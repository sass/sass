#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'haml/html'

class Html2HamlTest < Test::Unit::TestCase
  def test_empty_render_should_remain_empty
    assert_equal '', render('')
  end

  def test_id_and_class_should_be_removed_from_hash
    assert_equal '%span#foo.bar', render('<span id="foo" class="bar"> </span>')
  end

  def test_no_tag_name_for_div_if_class_or_id_is_present
    assert_equal '#foo', render('<div id="foo"> </div>')
    assert_equal '.foo', render('<div class="foo"> </div>')
  end

  def test_multiple_class_names
    assert_equal '.foo.bar.baz', render('<div class=" foo  bar  baz "> </div>')
  end

  def test_should_have_pretty_attributes
    assert_equal('%input{:name => "login", :type => "text"}/',
      render('<input type="text" name="login" />'))
    assert_equal('%meta{:content => "text/html", "http-equiv" => "Content-Type"}/',
      render('<meta http-equiv="Content-Type" content="text/html" />'))
  end

  def test_class_with_dot_and_hash
    assert_equal('%div{:class => "foo.bar"}', render("<div class='foo.bar'></div>"))
    assert_equal('%div{:class => "foo#bar"}', render("<div class='foo#bar'></div>"))
    assert_equal('.foo.bar{:class => "foo#bar foo.bar"}', render("<div class='foo foo#bar bar foo.bar'></div>"))
  end

  def test_id_with_dot_and_hash
    assert_equal('%div{:id => "foo.bar"}', render("<div id='foo.bar'></div>"))
    assert_equal('%div{:id => "foo#bar"}', render("<div id='foo#bar'></div>"))
  end

  def test_interpolation
    assert_equal('Foo \#{bar} baz', render('Foo #{bar} baz'))
  end

  def test_interpolation_in_attrs
    assert_equal('%p{:foo => "\#{bar} baz"}', render('<p foo="#{bar} baz"></p>'))
  end

  def test_cdata
    assert_equal(<<HAML.strip, render(<<HTML))
%p
  :cdata
    <a foo="bar" baz="bang">
    <div id="foo">flop</div>
    </a>
HAML
<p><![CDATA[
  <a foo="bar" baz="bang">
    <div id="foo">flop</div>
  </a>
]]></p>
HTML
  end

  def test_self_closing_tag
    assert_equal("%foo/", render("<foo />"))
  end

  def test_inline_text
    assert_equal("%p foo", render("<p>foo</p>"))
  end

  def test_inline_comment
    assert_equal("/ foo", render("<!-- foo -->"))
  end

  def test_non_inline_comment
    assert_equal(<<HAML.rstrip, render(<<HTML))
/
  Foo
  Bar
HAML
<!-- Foo
Bar -->
HTML
  end

  def test_non_inline_text
    assert_equal(<<HAML.rstrip, render(<<HTML))
%p
  foo
HAML
<p>
  foo
</p>
HTML
    assert_equal(<<HAML.rstrip, render(<<HTML))
%p
  foo
HAML
<p>
  foo</p>
HTML
    assert_equal(<<HAML.rstrip, render(<<HTML))
%p
  foo
HAML
<p>foo
</p>
HTML
  end

  def test_script_tag
    assert_equal(<<HAML.rstrip, render(<<HTML))
:javascript
  function foo() {
      return "12" & "13";
  }
HAML
<script type="text/javascript">
    function foo() {
        return "12" &amp; "13";
    }
</script>
HTML
  end

  def test_script_tag_with_cdata
    assert_equal(<<HAML.rstrip, render(<<HTML))
:javascript
  function foo() {
    return "&amp;";
  }
HAML
<script type="text/javascript">
  <![CDATA[
    function foo() {
      return "&amp;";
    }
  ]]>
</script>
HTML
  end

  def test_pre
    assert_equal(<<HAML.rstrip, render(<<HTML))
%pre
  :preserve
    foo
      bar
    baz
HAML
<pre>foo
  bar
baz</pre>
HTML
  end

  def test_pre_code
    assert_equal(<<HAML.rstrip, render(<<HTML))
%pre
  %code
    :preserve
      foo
        bar
      baz
HAML
<pre><code>foo
  bar
baz</code></pre>
HTML
  end

  def test_code_without_pre
    assert_equal(<<HAML.rstrip, render(<<HTML))
%code
  foo
  bar
  baz
HAML
<code>foo
  bar
baz</code>
HTML
  end

  def test_conditional_comment
    assert_equal(<<HAML.rstrip, render(<<HTML))
/[if foo]
  bar
  baz
HAML
<!--[if foo]>
  bar
  baz
<![endif]-->
HTML
  end

  def test_inline_conditional_comment
    assert_equal(<<HAML.rstrip, render(<<HTML))
/[if foo] bar baz
HAML
<!--[if foo]> bar baz <![endif]-->
HTML
  end

  ## ERB

  def test_erb
    assert_equal '- foo = bar', render_erb('<% foo = bar %>')
    assert_equal '- foo = bar', render_erb('<% foo = bar -%>')
    assert_equal '= h @item.title', render_erb('<%=h @item.title %>')
    assert_equal '= h @item.title', render_erb('<%=h @item.title -%>')
  end

  def test_inline_erb
    assert_equal("%p= foo", render_erb("<p><%= foo %></p>"))
  end

  def test_non_inline_erb
    assert_equal(<<HAML.rstrip, render_erb(<<HTML))
%p
  = foo
HAML
<p>
  <%= foo %>
</p>
HTML
    assert_equal(<<HAML.rstrip, render_erb(<<HTML))
%p
  = foo
HAML
<p>
  <%= foo %></p>
HTML
    assert_equal(<<HAML.rstrip, render_erb(<<HTML))
%p
  = foo
HAML
<p><%= foo %>
</p>
HTML
  end

  def test_erb_in_cdata
    assert_equal(<<HAML.rstrip, render_erb(<<HTML))
:cdata
  Foo \#{bar} baz
HAML
<![CDATA[Foo <%= bar %> baz]]>
HTML
  end

  def test_erb_in_script
    assert_equal(<<HAML.rstrip, render_erb(<<HTML))
:javascript
  function foo() {
    return \#{foo.to_json};
  }
HAML
<script type="text/javascript">
  function foo() {
    return <%= foo.to_json %>;
  }
</script>
HTML
  end

  def test_erb_in_line
    assert_equal 'foo bar #{baz}', render_erb('foo bar <%= baz %>')
    assert_equal 'foo bar #{baz}! Bang.', render_erb('foo bar <%= baz %>! Bang.')
  end

  def test_erb_multi_in_line
    assert_equal('foo bar #{baz}! Bang #{bop}.',
      render_erb('foo bar <%= baz %>! Bang <%= bop %>.'))
    assert_equal('foo bar #{baz}#{bop}!',
      render_erb('foo bar <%= baz %><%= bop %>!'))
  end

  def test_erb_with_html_special_chars
    assert_equal '= 3 < 5 ? "OK" : "Your computer is b0rken"',
      render_erb('<%= 3 < 5 ? "OK" : "Your computer is b0rken" %>')
  end

  def test_erb_in_class_attribute
    assert_equal "%div{:class => dyna_class} I have a dynamic attribute",
      render_erb('<div class="<%= dyna_class %>">I have a dynamic attribute</div>')
  end

  def test_erb_in_id_attribute
    assert_equal "%div{:id => dyna_id} I have a dynamic attribute",
      render_erb('<div id="<%= dyna_id %>">I have a dynamic attribute</div>')
  end

  def test_erb_in_attribute_results_in_string_interpolation
    assert_equal('%div{:id => "item_#{i}"} Ruby string interpolation FTW',
      render_erb('<div id="item_<%= i %>">Ruby string interpolation FTW</div>'))
  end

  def test_erb_in_attribute_with_trailing_content
    assert_equal('%div{:class => "#{12}!"} Bang!',
      render_erb('<div class="<%= 12 %>!">Bang!</div>'))
  end

  def test_erb_in_html_escaped_attribute
    assert_equal '%div{:class => "foo"} Bang!',
      render_erb('<div class="<%= "foo" %>">Bang!</div>')
  end

  def test_erb_in_attribute_to_multiple_interpolations
    assert_equal('%div{:class => "#{12} + #{13}"} Math is super',
      render_erb('<div class="<%= 12 %> + <%= 13 %>">Math is super</div>'))
  end

  def test_whitespace_eating_erb_tags
    assert_equal '- form_for', render_erb('<%- form_for -%>')
  end

  def test_interpolation_in_erb
    assert_equal('= "Foo #{bar} baz"', render_erb('<%= "Foo #{bar} baz" %>'))
  end

  def test_interpolation_in_erb_attrs
    assert_equal('%p{:foo => "#{bar} baz"}',
      render_erb('<p foo="<%= "#{bar} baz" %>"></p>'))
  end

  def test_multiline_erb_silent_script
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
.blah
  - foo
  - bar
  - baz
  %p foo
HAML
<div class="blah">
  <%
    foo
    bar
    baz
  %>
  <p>foo</p>
</div>
ERB
  end

  ### Block Parsing

  def test_block_parsing
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- foo do
  %p bar
HAML
<% foo do %>
  <p>bar</p>
<% end %>
ERB
  end

  def test_block_parsing_with_args
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- foo do |a, b, c|
  %p bar
HAML
<% foo do |a, b, c| %>
  <p>bar</p>
<% end %>
ERB
  end

  def test_block_parsing_with_equals
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
= foo do
  %p bar
HAML
<%= foo do %>
  <p>bar</p>
<% end %>
ERB
  end

  def test_block_parsing_with_modified_end
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- foo do
  blah
- end.bip
HAML
<% foo do %>
  blah
<% end.bip %>
ERB
  end

  def test_block_parsing_with_modified_end_with_block
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- foo do
  blah
- end.bip do
  brang
HAML
<% foo do %>
  blah
<% end.bip do %>
  brang
<% end %>
ERB
  end

  def test_multiline_block_opener
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- foo bar
- baz bang
- biddle do
  foo
HAML
<% foo bar
  baz bang
  biddle do %>
    foo
<% end %>
ERB
  end

  def test_if_elsif_else_parsing
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- if foo
  %p bar
- elsif bar.foo("zip")
  #bang baz
- else
  %strong bibble
HAML
<% if foo %>
  <p>bar</p>
<% elsif bar.foo("zip") %>
  <div id="bang">baz</div>
<% else %>
  <strong>bibble</strong>
<% end %>
ERB
  end

  def test_case_when_parsing
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- case foo.bar
- when "bip"
  %p bip
- when "bop"
  %p BOP
- when bizzle.bang.boop.blip
  %em BIZZLE BANG BOOP BLIP
HAML
<% case foo.bar %>
<% when "bip" %>
  <p>bip</p>
<% when "bop" %>
  <p>BOP</p>
<% when bizzle.bang.boop.blip %>
  <em>BIZZLE BANG BOOP BLIP</em>
<% end %>
ERB

    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- case foo.bar
- when "bip"
  %p bip
- when "bop"
  %p BOP
- when bizzle.bang.boop.blip
  %em BIZZLE BANG BOOP BLIP
HAML
<% case foo.bar
   when "bip" %>
  <p>bip</p>
<% when "bop" %>
  <p>BOP</p>
<% when bizzle.bang.boop.blip %>
  <em>BIZZLE BANG BOOP BLIP</em>
<% end %>
ERB
  end

  def test_begin_rescue_ensure
    assert_equal(<<HAML.rstrip, render_erb(<<ERB))
- begin
  %p a
- rescue FooException => e
  %p b
- ensure
  %p c
HAML
<% begin %>
  <p>a</p>
<% rescue FooException => e %>
  <p>b</p>
<% ensure %>
  <p>c</p>
<% end %>
ERB
  end

  # Encodings

  unless Haml::Util.ruby1_8?
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
  end

  protected

  def render(text, options = {})
    Haml::HTML.new(text, options).render.rstrip
  end

  def render_erb(text)
    render(text, :erb => true)
  end
end
