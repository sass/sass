#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/helpers'

class HelperTest < Test::Unit::TestCase
  include Haml::Helpers

  def render(text, options = {})
    Haml::Engine.new(text, options).to_html
  end

  def test_flatten
    assert_equal(flatten("FooBar"), "FooBar")

    assert_equal(flatten("Foo\rBar"), "FooBar")

    assert_equal(flatten("Foo\nBar"), "Foo&#x000A;Bar")

    assert_equal(flatten("Hello\nWorld!\nYOU ARE \rFLAT?\n\rOMGZ!"),
                         "Hello&#x000A;World!&#x000A;YOU ARE FLAT?&#x000A;OMGZ!")
  end

  def test_list_of_should_render_correctly
    assert_equal("<li>1</li>\n<li>2</li>", (list_of([1, 2]) { |i| i.to_s}))
    assert_equal("<li>1</li>", (list_of([[1]]) { |i| i.first}))
  end

  def test_buffer_access
    assert(render("= buffer") =~ /#<Haml::Buffer:0x[a-z0-9]+>/)
    assert_equal(render("= (buffer == _hamlout)"), "true\n")
  end

  def test_tabs
    assert_equal(render("foo\n- tab_up\nbar\n- tab_down\nbaz"), "foo\n  bar\nbaz\n")
  end
  
  def test_helpers_dont_leak
    # Haml helpers shouldn't be accessible from ERB
    render("foo")
    proper_behavior = false

    begin
      ActionView::Base.new.render(:inline => "<%= flatten('Foo\\nBar') %>")
    rescue NoMethodError
      proper_behavior = true
    end
    assert(proper_behavior)

    begin
      ActionView::Base.new.render(:inline => "<%= concat('foo') %>")
    rescue ArgumentError
      proper_behavior = true
    end    
    assert(proper_behavior)
  end
  
  def test_action_view_included
    assert(Haml::Helpers.action_view?)
  end
  
  def test_action_view_not_included
    #This is for 100% rcov, rather than any real testing purposes.
    Kernel.module_eval do
      alias_method :old_require, :require
      def require(string)
        raise LoadError if string == "action_view"
        old_require string
      end
    end

    load File.dirname(__FILE__) + '/../lib/haml/helpers/action_view_mods.rb'

    Kernel.module_eval do
      alias_method :require, :old_require
    end
  end
end

