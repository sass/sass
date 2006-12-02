#!/usr/bin/env ruby

require 'test/unit'
require 'rubygems'
require 'active_support'
require 'action_view'

require File.dirname(__FILE__) + '/../lib/haml/template'
require File.dirname(__FILE__) + '/mocks/article'

class TemplateTest < Test::Unit::TestCase
  @@templates = %w{       very_basic        standard    helpers
    whitespace_handling   original_engine   list        helpful
    silent_script         tag_parsing       just_stuff  partials }

  def setup
    ActionView::Base.register_template_handler("haml", Haml::Template)
    @base = ActionView::Base.new(File.dirname(__FILE__) + "/../test/templates/")
    @base.instance_variable_set("@article", Article.new)
    @base.instance_variable_set("@foo", 'value one')
  end

  def render(text)
    Haml::Engine.new(text).to_html(@base)
  end

  def load_result(name)
    @result = ''
    File.new(File.dirname(__FILE__) + "/results/#{name}.xhtml").each_line { |l| @result += l }
    @result
  end

  def assert_renders_correctly(name)
    test = Proc.new do |rendered|
      load_result(name).split("\n").zip(rendered.split("\n")).each_with_index do |pair, line|
        message = "template: #{name}\nline:     #{line}"
        assert_equal(pair.first, pair.last, message)
      end
    end
    test.call(@base.render(name))
    test.call(@base.render(:file => "partialize", :locals => { :name => name }))
  end

  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  def test_templates_should_render_correctly
    @@templates.each do |template|
      assert_renders_correctly template
    end
  end

  def test_action_view_templates_render_correctly
    @base.instance_variable_set("@content_for_layout", 'Lorem ipsum dolor sit amet')
    assert_renders_correctly 'content_for_layout'
  end

  def test_instance_variables_should_work_inside_templates
    @base.instance_variable_set("@content_for_layout", 'something')
    assert_equal("<p>something</p>", render("%p= @content_for_layout").chomp)

    @base.instance_eval("@author = 'Hampton Catlin'")
    assert_equal("<div class='author'>Hampton Catlin</div>", render(".author= @author").chomp)

    @base.instance_eval("@author = 'Hampton'")
    assert_equal("Hampton", render("= @author").chomp)

    @base.instance_eval("@author = 'Catlin'")
    assert_equal("Catlin", render("= @author").chomp)
  end

  def test_instance_variables_should_work_inside_attributes
    @base.instance_eval("@author = 'hcatlin'")
    assert_equal("<p class='hcatlin'>foo</p>", render("%p{:class => @author} foo").chomp)
  end

  def test_template_renders_should_eval
    assert_equal("2\n", render("= 1+1"))
  end

  def test_rhtml_still_renders
    res = @base.render("../rhtml/standard")
    assert !(res.nil? || res.empty?)
  end

  def test_haml_options
    Haml::Template.options = { :suppress_eval => true }
    assert_equal({ :suppress_eval => true }, Haml::Template.options)
    assert_renders_correctly("eval_suppressed")
    Haml::Template.options = {}
  end

  def test_exceptions_should_work_correctly
    template = <<END
%p
  %h1 Hello!
  = "lots of lines"
  - raise "Oh no!"
  %p
    this is after the exception
    %strong yes it is!
ho ho ho.
END
    @base.haml_filename = "(test)"
    begin
      render(template.chomp)
    rescue Exception => e
      assert_equal("(test).haml:4", e.backtrace[0])
    end

    @base.haml_filename = nil
    begin
      render(template.chomp)
    rescue Exception => e
      assert_equal("(haml):4", e.backtrace[0])
    end

    template = <<END
%p
  %h1 Hello!
  = "lots of lines"
  = "even more!"
  - compile_error(
  %p
    this is after the exception
    %strong yes it is!
ho ho ho.
END

    begin
      render(template.chomp)
    rescue Exception => e
      assert_equal("(haml):5", e.backtrace[0])
    end
  end
end
