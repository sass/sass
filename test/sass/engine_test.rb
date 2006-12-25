#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/sass'
require 'sass/engine'

class SassEngineTest < Test::Unit::TestCase
  EXCEPTION_MAP = {
    "!a = 1 + " => "Syntax error:\n1 +",
    "!a = 1 + 2 +" => "Syntax error:\n1 + 2 +",
    "!a = #aaa - a" => "Undefined operation:\n#afafaf minus a",
    "!a = #aaa / a" => "Undefined operation:\n#afafaf div a",
    "!a = #aaa * a" => "Undefined operation:\n#afafaf times a",
    "!a = #aaa % a" => "Undefined operation:\n#afafaf mod a",
    "!a = 1 - a" => "Undefined operation:\n1 minus a",
    "!a = 1 * a" => "Undefined operation:\n1 times a",
    "!a = 1 / a" => "Undefined operation:\n1 div a",
    "!a = 1 % a" => "Undefined operation:\n1 mod a",
  }
  
  def test_basic_render
    renders_correctly "basic"
  end
  
  def test_exceptions
    EXCEPTION_MAP.each do |key, value|
      val_lines = value.split("\n")
      begin
        Sass::Engine.new(key).render
      rescue RuntimeError => res_lines; end
      val_lines.zip(res_lines.to_s.split("\n")) { |val, res| assert_equal(val, res) }
    end
  end
  
  private

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
