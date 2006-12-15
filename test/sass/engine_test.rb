#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'
require 'sass/engine'

class SassEngineTest < Test::Unit::TestCase
  
  def test_basic_render
    renders_correctly "basic"
  end

  def renders_correctly(name)
    sass_file  = load_file(name, "sass")
    css_file   = load_file(name, "css")
    css_result = Sass::Engine.new(sass_file).render
    #puts css_result.collect { |a| a.inspect }.join("\n  ")
    assert_equal css_file, css_result
  end

  def load_file(name, type = "sass")
    @result = ''
    File.new(File.dirname(__FILE__) + "/#{type == 'sass' ? 'templates' : 'results'}/#{name}.#{type}").each_line { |l| @result += l }
    @result
  end
end
