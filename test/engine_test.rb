#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/engine'

class EngineTest < Test::Unit::TestCase

  def render(text, options = {})
    Haml::Engine.new(text, options).to_html
  end
  
  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  def test_stop_eval
    assert_equal("", render("= 'Hello'", :suppress_eval => true))
  end

  # This is ugly because Hashes are unordered; we don't always know the order
  # in which attributes will be returned.
  # There is probably a better way to do this.
  def test_attributes_should_render_correctly
    assert_equal("<div class='atlantis' style='ugly'>\n</div>", render(".atlantis{:style => 'ugly'}").chomp)
  rescue
    assert_equal("<div style='ugly' class='atlantis'>\n</div>", render(".atlantis{:style => 'ugly'}").chomp)
  end

  def test_ruby_code_should_work_inside_attributes
    author = 'hcatlin'
    assert_equal("<p class='3'>foo</p>", render("%p{:class => 1+2} foo").chomp)
  end

  def test_nil_should_render_empty_tag
    assert_equal("<div class='no_attributes'>\n</div>",
                 render(".no_attributes{:nil => nil}").chomp)
  end

  def test_strings_should_get_stripped_inside_tags
    assert_equal("<div class='stripped'>This should have no spaces in front of it</div>",
                 render(".stripped    This should have no spaces in front of it").chomp)
  end
  
  def test_one_liner_should_be_one_line
    assert_equal("<p>Hello</p>", render('%p Hello').chomp)
  end

  def test_long_liner_should_not_print_on_one_line
    assert_equal("<div>\n  #{'x' * 51}\n</div>", render("%div #{'x' * 51}").chomp)
  end
end
