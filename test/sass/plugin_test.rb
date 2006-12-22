#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'

RAILS_ROOT = ''
RAILS_ENV  = 'testing'

require 'sass/plugin'

class SassPluginTest < Test::Unit::TestCase
  @@templates = %w{ complex constants }

  def setup
    Sass::Plugin.options[:template_location]  = File.dirname(__FILE__) + '/templates'
    Sass::Plugin.options[:css_location]       = File.dirname(__FILE__) + '/tmp'
    Sass::Plugin.options = {:always_update => true}
    
    Sass::Plugin.update_stylesheets
  end
  
  def teardown
    File.delete(*Dir[tempfile_loc('*')])
  end

  def test_templates_should_render_correctly
    @@templates.each { |name| assert_renders_correctly(name) }
  end
  
  def test_no_update
    File.delete(tempfile_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('basic')
    Sass::Plugin.update_stylesheets
    assert !Sass::Plugin.stylesheet_needs_update?('basic')
  end
  
  def assert(*args)
  end
  
  def test_controller_process
    File.delete(tempfile_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('basic')
    
    ActionController::Base.new.process
    
    assert !Sass::Plugin.stylesheet_needs_update?('basic')
  end

 private
 
  def assert_renders_correctly(name)
    File.read(result_loc(name)).split("\n").zip(File.read(tempfile_loc(name)).split("\n")).each_with_index do |pair, line|
      message = "template: #{name}\nline:     #{line}"
      assert_equal(pair.first, pair.last, message)
    end
  end
  
  def tempfile_loc(name)
    File.dirname(__FILE__) + "/tmp/#{name}.css"
  end
  
  def result_loc(name)
    File.dirname(__FILE__) + "/results/#{name}.css"
  end
end

module Sass::Plugin
  class << self
    public :stylesheet_needs_update?
  end
end

class ActionController::Base
  def sass_old_process(*args); end
end
