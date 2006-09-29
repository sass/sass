require 'test/unit'
require 'rubygems'
require 'action_view'

require File.dirname(__FILE__) + '/../lib/haml/template'
require File.dirname(__FILE__) + '/mocks/article'

class TemplateTest < Test::Unit::TestCase
  def setup
    ActionView::Base.register_template_handler("haml", Haml::Template)
    @base = ActionView::Base.new(File.dirname(__FILE__) + "/../test/templates/")
    @base.instance_variable_set("@article", Article.new)
  end

  def render(text)
    Haml::Engine.new(text, @base).to_html
  end

  def load_result(name)
    @result = ''
    File.new(File.dirname(__FILE__) + "/results/#{name}.xhtml").each_line { |l| @result += l }
    @result
  end

  def assert_renders_correctly(name)
    load_result(name).split("\n").zip(@base.render(name).split("\n")).each do |pair|
      assert_equal(pair.first, pair.last)
    end
  end

  def test_empty_render_should_remain_empty
    assert_equal('', render(''))
  end

  def test_templates_should_render_correctly
    %w{very_basic standard helpers whitespace_handling original_engine list helpful}.each do |template|
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
end
