#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass/engine'
require File.dirname(__FILE__) + '/../../lib/sass/sass_helper'
require File.dirname(__FILE__) + '/mocks/fake_controller'

class SassPluginTest < Test::Unit::TestCase
  def setup
    Sass::SassHelper.options[:stylesheet_location] = File.dirname(__FILE__) + '/templates'
    Sass::SassHelper.options[:always_update]       = true
    @controller                                = FakeController.new
  end
  
  def teardown
    File.delete(*Dir[File.dirname(__FILE__) + '/templates/*.css'])
  end

  def test_basic_render
    do_file_render    :basic
  end

  def test_complex_render
    do_file_render :complex
  end

  def test_no_update
    base_file_location = base_file_location(:basic)
    assert Sass::SassHelper.stylesheet_needs_update?(base_file_location)
    do_file_render :basic
    assert !Sass::SassHelper.stylesheet_needs_update?(base_file_location)
  end

 private
 
  def assert_renders_correctly(name)
    do_file_render(name)
    
    tempfile  = File.open('/templates/#{name}.css')
    result    = File.open('/results/#{name}.css')
    
    tempfile.read.split("\n").zip(result.read.split("\n")).each_with_index do |pair, line|
      message = "template: #{name}\nline:     #{line}"
      assert_equal(pair.first, pair.last, message)
    end
    
    tempfile.close
    result.close
    
    kill_tempfile(name)
  end
  
  def do_file_render(name)
    @controller.instance_eval do
      sass_template(name)
    end
  end

  def base_file_location(name)
    File.dirname(__FILE__) + "/templates/#{name}"
  end
  
  def kill_tempfile(name)
    File.delete(File.dirname(__FILE__) + '/templates/#{name}.css')
  end
end
