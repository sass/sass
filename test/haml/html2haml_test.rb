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
    assert_equal('%input{:name => "login", :type => "text"}',
      render('<input type="text" name="login" />'))
    assert_equal('%meta{:content => "text/html", "http-equiv" => "Content-Type"}',
      render('<meta http-equiv="Content-Type" content="text/html" />'))
  end

  def test_sqml_comment
    assert_equal "/\n  IE sucks", render('<!-- IE sucks -->')
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

  def test_inline_text
    assert_equal("%p foo", render("<p>foo</p>"))
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
