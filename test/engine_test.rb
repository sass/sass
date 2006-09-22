require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/engine'
require File.dirname(__FILE__) + '/mocks/article'

$:.unshift File.join(File.dirname(__FILE__), "..", "lib")

require 'rubygems'
require 'action_view'

class HamlTest < Test::Unit::TestCase
  include Haml::Helpers

  def setup
    ActionView::Base.register_template_handler("haml", Haml::Engine)
    @base = ActionView::Base.new(File.dirname(__FILE__) + "/templates/")
    @engine = Haml::Engine.new(@base)
    @base.instance_variable_set("@article", Article.new)
  end

  def render(text)
    @engine.render(text)
  end

  def load_result(name)
    @result = ''
    File.new(File.dirname(__FILE__) + "/results/#{name}.xhtml").each_line { |l| @result += l}
    @result
  end

  def assert_renders_correctly(name)
    load_result(name).split("\n").zip(@base.render(name).split("\n")).each do |pair|
      assert_equal(pair.first, pair.last)
      #puts pair.inspect
    end
  end

  # Make sure our little environment builds
  def test_build_stub
    assert_not_nil(@engine)
    assert @engine.is_a?(Haml::Engine)
    assert_equal(Haml::Engine, @engine.class)
  end

  def test_empty_render
    assert_equal('', render(''))
  end

  def test_renderings
    assert_renders_correctly("very_basic")
    assert_renders_correctly("standard")
    assert_renders_correctly("helpers")
    assert_renders_correctly("whitespace_handling")
    assert_renders_correctly("original_engine")
    assert_renders_correctly("list")
    assert_renders_correctly("helpful")
  end

  def test_instance_variables
    @base.instance_eval("@content_for_layout = 'Hampton'")
    @base.instance_eval("@assigns['last_name'] = 'Catlin'")
    #make sure to reload!
    @engine = Haml::Engine.new(@base)
    assert_equal("<div class='author'>Hampton Catlin</div>\n", render(".author= @content_for_layout + ' ' + @last_name"))
  end

  def test_instance_variables_changing
    @base.instance_eval("@author = 'Hampton'")
    assert_equal("Hampton\n", render("= @author"))
  end

  def test_nil_attribute
    assert_equal("<div class='no_attributes'>\n</div>\n",
                 render(".no_attributes{:nil => nil}"))
  end

  def test_stripped_strings
    assert_equal("<div class='stripped'>This should have no spaces in front of it</div>\n",
                 render(".stripped    This should have no spaces in front of it"))
  end
end
