#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass/engine'

class SassEngineTest < Test::Unit::TestCase
  def setup
    @engine = Sass::Engine.new
  end
  
  def test_basic_render
    renders_correctly "basic"
  end

  def renders_correctly(name)
    sass_file  = load_file(name, "sass")
    css_file   = load_file(name, "css")
    css_result = @engine.render(sass_file)
    #puts css_result.collect { |a| a.inspect }.join("\n  ")
    assert_equal css_file, css_result
  end

  def load_file(name, type = "sass")
    @result = ''
    File.new(File.dirname(__FILE__) + "/#{type == 'sass' ? 'templates' : 'results'}/#{name}.#{type}").each_line { |l| @result += l }
    @result
  end
end
