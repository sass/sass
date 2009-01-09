#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/plugin'
require 'fileutils'

class SassPluginTest < Test::Unit::TestCase
  @@templates = %w{
    complex constants parent_ref import alt
    subdir/subdir subdir/nested_subdir/nested_subdir
  }

  def setup
    FileUtils.mkdir File.dirname(__FILE__) + '/tmp'
    set_plugin_opts
    Sass::Plugin.update_stylesheets
  end

  def teardown
    FileUtils.rm_r File.dirname(__FILE__) + '/tmp'
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

  def test_update_needed_when_modified
    sleep(1)
    FileUtils.touch(template_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('basic')
    Sass::Plugin.update_stylesheets
    assert !Sass::Plugin.stylesheet_needs_update?('basic')
  end

  def test_update_needed_when_dependency_modified
    sleep(1)
    FileUtils.touch(template_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('import')
    Sass::Plugin.update_stylesheets
    assert !Sass::Plugin.stylesheet_needs_update?('import')
  end

  def test_full_exception_handling
    File.delete(tempfile_loc('bork'))
    Sass::Plugin.update_stylesheets
    File.open(tempfile_loc('bork')) do |file|
      assert_equal("/*\nSass::SyntaxError: Undefined constant: \"!bork\".\non line 2 of #{File.dirname(__FILE__) + '/templates/bork.sass'}\n\n1: bork\n2:   :bork= !bork", file.read.split("\n")[0...6].join("\n"))
    end
    File.delete(tempfile_loc('bork'))
  end

  def test_nonfull_exception_handling
    Sass::Plugin.options[:full_exception] = false

    File.delete(tempfile_loc('bork'))
    Sass::Plugin.update_stylesheets
    assert_equal("/* Internal stylesheet error */", File.read(tempfile_loc('bork')))
    File.delete(tempfile_loc('bork'))

    Sass::Plugin.options[:full_exception] = true
  end

  def test_rails_update    
    File.delete(tempfile_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('basic')

    ActionController::Base.new.process

    assert !Sass::Plugin.stylesheet_needs_update?('basic')
  end

  def test_merb_update
    begin
      require 'merb'
    rescue LoadError
      puts "\nmerb couldn't be loaded, skipping a test"
      return
    end
    
    require 'sass/plugin/merb'
    if defined?(MerbHandler)
      MerbHandler.send(:define_method, :process_without_sass) { |*args| }
    else
      Merb::Rack::Application.send(:define_method, :call_without_sass) { |*args| }
    end

    set_plugin_opts

    File.delete(tempfile_loc('basic'))
    assert Sass::Plugin.stylesheet_needs_update?('basic')
    
    if defined?(MerbHandler)
      MerbHandler.new('.').process nil, nil
    else
      Merb::Rack::Application.new.call(::Rack::MockRequest.env_for('/'))
    end

    assert !Sass::Plugin.stylesheet_needs_update?('basic')
  end

  def test_doesnt_render_partials
    assert !File.exists?(tempfile_loc('_partial'))
  end

 private

  def assert_renders_correctly(name)
    File.read(result_loc(name)).split("\n").zip(File.read(tempfile_loc(name)).split("\n")).each_with_index do |pair, line|
      message = "template: #{name}\nline:     #{line + 1}"
      assert_equal(pair.first, pair.last, message)
    end
  end

  def template_loc(name)
    File.dirname(__FILE__) + "/templates/#{name}.sass"
  end

  def tempfile_loc(name)
    File.dirname(__FILE__) + "/tmp/#{name}.css"
  end

  def result_loc(name)
    File.dirname(__FILE__) + "/results/#{name}.css"
  end

  def set_plugin_opts
    Sass::Plugin.options = {
      :template_location => File.dirname(__FILE__) + '/templates',
      :css_location => File.dirname(__FILE__) + '/tmp',
      :style => :compact,
      :load_paths => [File.dirname(__FILE__) + '/results'],
      :always_update => true,
    }
  end
end

module Sass::Plugin
  class << self
    public :stylesheet_needs_update?
  end
end

class Sass::Engine
  alias_method :old_render, :render

  def render
    raise "bork bork bork!" if @template[0] == "{bork now!}"
    old_render
  end
end

class ActionController::Base
  undef :sass_old_process
  def sass_old_process(*args); end
end
