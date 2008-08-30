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
    assert_equal_attributes('%input{ :type => "text", :name => "login" }/',
                            render('<input type="text" name="login" />'))
    assert_equal_attributes('%meta{ "http-equiv" => "Content-Type", :content => "text/html" }/',
                            render('<meta http-equiv="Content-Type" content="text/html" />'))
    assert_equal_attributes('%div{ "xml:lang" => "hr" }/',
                            render('<div xml:lang="hr" />'))
  end

  def test_sqml_comment
    assert_equal "/\n  IE sucks", render('<!-- IE sucks -->')
  end

  def test_rhtml
    assert_equal '- foo = bar', render_rhtml('<% foo = bar %>')
    assert_equal '- foo = bar', render_rhtml('<% foo = bar -%>')
    assert_equal '= h @item.title', render_rhtml('<%=h @item.title %>')
    assert_equal '= h @item.title', render_rhtml('<%=h @item.title -%>')
  end

  protected

    def render(text, options = {})
      Haml::HTML.new(text, options).render.rstrip
    end

    def render_rhtml(text)
      render(text, :rhtml => true)
    end

    def assert_equal_attributes(expected, result)
      expected_attr, result_attr = [expected, result].map { |s| s.gsub!(/\{ (.+) \}/, ''); $1.split(', ').sort }
      assert_equal expected_attr, result_attr
      assert_equal expected, result
    end
end
