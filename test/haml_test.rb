require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/engine'
$:.unshift File.join(File.dirname(__FILE__), "..", "lib")
require 'rubygems'
require 'action_view'

class HamlTest < Test::Unit::TestCase
  def setup
    ActionView::Base.register_template_handler("haml", HAML::Engine)
    @base = ActionView::Base.new(File.dirname(__FILE__) + "/../test/templates/")
    @engine = HAML::Engine.new(@base)
  end

  def load_result(name)
    @result = ""
    File.new(File.dirname(__FILE__) + "/results/" + name + ".xhtml").each_line { |l| @result += l}
    @result
  end

  def assert_renders_correctly(name)
    assert_equal(load_result(name), @base.render(name))
  end

  # Make sure our little environment builds
  def test_build_stub
    assert_not_nil(@engine)
    assert_equal(HAML::Engine, @engine.class)
  end

  def test_empty_render
    assert_equal("", @engine.render(""))
  end

  def test_renderings
    assert_renders_correctly("very_basic")
    assert_renders_correctly("standard")
    assert_renders_correctly("helpers")
  end

  def test_instance_variables
    @base.instance_eval("@content_for_layout = 'Hampton'")
    @base.instance_eval("@assigns['last_name'] = 'Catlin'")
    #make sure to reload!
    @engine = HAML::Engine.new(@base)
    assert_equal("<div class='author'>Hampton Catlin</div>\n", @engine.render(".author= @content_for_layout + ' ' + @last_name"))
  end

end
