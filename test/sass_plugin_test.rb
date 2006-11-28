#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../lib/sass/engine'
require File.dirname(__FILE__) + '/../lib/sass/plugin'
require File.dirname(__FILE__) + '/mocks/fake_controller'

class SassPluginTest < Test::Unit::TestCase
  def setup
    Sass::Plugin.options[:stylesheet_location] = File.dirname(__FILE__) + '/sass'
    Sass::Plugin.options[:always_update]       = true
    @controller = FakeController.new
  end
  
  def test_truth
    @controller.instance_eval do
      sass_template(:basic)
    end

    assert FileUtils.compare_file(File.dirname(__FILE__) + "/sass/basic.css",
                                  File.dirname(__FILE__) + "/css/basic.css")
    FileUtils.rm(File.dirname(__FILE__) + "/sass/basic.css")
  end
end
