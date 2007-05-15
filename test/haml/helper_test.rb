#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/haml'
require 'haml/template'

class HelperTest < Test::Unit::TestCase
  include Haml::Helpers
  
  def setup
    ActionView::Base.register_template_handler("haml", Haml::Template)
    @base = ActionView::Base.new
    @base.controller = ActionController::Base.new
  end

  def render(text, options = {})
    if options == :action_view
      @base.render :inline => text, :type => :haml
    else
      scope = options.delete :scope_object
      Haml::Engine.new(text, options).to_html(scope ? scope : Object.new)
    end
  end

  def test_flatten
    assert_equal(flatten("FooBar"), "FooBar")

    assert_equal(flatten("Foo\rBar"), "FooBar")

    assert_equal(flatten("Foo\nBar"), "Foo&#x000A;Bar")

    assert_equal(flatten("Hello\nWorld!\nYOU ARE \rFLAT?\n\rOMGZ!"),
                         "Hello&#x000A;World!&#x000A;YOU ARE FLAT?&#x000A;OMGZ!")
  end

  def test_list_of_should_render_correctly
    assert_equal("<li>1</li>\n<li>2</li>\n", render("= list_of([1, 2]) do |i|\n  = i"))
    assert_equal("<li>[1]</li>\n", render("= list_of([[1]]) do |i|\n  = i.inspect"))
    assert_equal("<li>\n  <h1>Fee</h1>\n  <p>A word!</p>\n</li>\n<li>\n  <h1>Fi</h1>\n  <p>A word!</p>\n</li>\n<li>\n  <h1>Fo</h1>\n  <p>A word!</p>\n</li>\n<li>\n  <h1>Fum</h1>\n  <p>A word!</p>\n</li>\n",
      render("= list_of(['Fee', 'Fi', 'Fo', 'Fum']) do |title|\n  %h1= title\n  %p A word!"))
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
    rescue ArgumentError, NameError
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

    load File.dirname(__FILE__) + '/../../lib/haml/helpers/action_view_mods.rb'

    Kernel.module_eval do
      alias_method :require, :old_require
    end
  end
  
  def test_form_tag
    result = render("- form_tag 'foo' do\n  %p bar\n  %strong baz", :action_view)
    should_be = "<form action=\"foo\" method=\"post\">\n  <p>bar</p>\n  <strong>baz</strong>\n</form>\n"
    assert_equal(should_be, result)
  end
  
  def test_capture_haml
    assert_equal("\"<p>13</p>\\n\"\n", render("- foo = capture_haml(13) do |a|\n  %p= a\n= foo.dump"))
  end

  def test_is_haml
    assert(!ActionView::Base.new.is_haml?)
    assert_equal("true\n", render("= is_haml?"))
    assert_equal("true\n", render("= is_haml?", :action_view))
    assert_equal("false", @base.render(:inline => '<%= is_haml? %>'))
    assert_equal("false\n", render("= render :inline => '<%= is_haml? %>'", :action_view))
  end

  def test_page_class
    controller = Struct.new(:controller_name, :action_name).new('troller', 'tion')
    scope = Struct.new(:controller).new(controller)
    result = render("%div{:class => page_class} MyDiv", :scope_object => scope)
    expected = "<div class='troller tion'>MyDiv</div>\n"
    assert_equal expected, result
  end

  def test_indented_capture
    assert_equal("  \n  Foo\n  ", @base.render(:inline => "  <% res = capture do %>\n  Foo\n  <% end %><%= res %>"))
  end

  def test_capture_deals_properly_with_collections
    Haml::Helpers.module_eval do 
      def trc(collection, &block)
        collection.each do |record|
          puts capture_haml(record, &block)
        end
      end
    end

    assert_equal("1\n\n2\n\n3\n\n", render("- trc([1, 2, 3]) do |i|\n  = i.inspect"))
  end
end

