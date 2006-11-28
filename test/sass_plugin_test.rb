#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../lib/sass/engine'
require File.dirname(__FILE__) + '/../lib/sass/plugin'
require File.dirname(__FILE__) + '/mocks/fake_controller'

class SassPluginTest < Test::Unit::TestCase
  def setup
    Sass::Plugin.options[:stylesheet_location] = File.dirname(__FILE__) + '/sass'
    Sass::Plugin.options[:always_update]       = true
    @controller                                = FakeController.new
  end

  def test_basic_render
    do_file_render    :basic
    clear_file_render :basic
  end

  def test_no_update
    base_file_location = base_file_location(:basic)
    assert Sass::Plugin.stylesheet_needs_update?(base_file_location)
    do_file_render :basic
    assert !Sass::Plugin.stylesheet_needs_update?(base_file_location)
  end

 private
  
  def do_file_render(name)
    @controller.instance_eval do
      sass_template(name)
    end

    assert FileUtils.compare_file(File.dirname(__FILE__) + "/sass/#{name}.css",
                                  File.dirname(__FILE__) + "/css/#{name}.css")
  end

  def base_file_location(name)
    File.dirname(__FILE__) + "/sass/#{name}"
  end

  def clear_file_render(name)
    FileUtils.rm(File.dirname(__FILE__) + "/sass/#{name}.css")
  end
end
