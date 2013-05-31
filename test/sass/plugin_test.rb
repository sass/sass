#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/plugin'
require 'fileutils'

module Sass::Script::Functions
  def filename
    filename = options[:filename].gsub(%r{.*((/[^/]+){4})}, '\1')
    Sass::Script::String.new(filename)
  end

  def whatever
    custom = options[:custom]
    whatever = custom && custom[:whatever]
    Sass::Script::String.new(whatever || "incorrect")
  end
end

class SassPluginTest < Test::Unit::TestCase
  @@templates = %w{
    complex script parent_ref import scss_import alt
    subdir/subdir subdir/nested_subdir/nested_subdir
    options import_content filename_fn
  }
  @@templates += %w[import_charset import_charset_ibm866] unless Sass::Util.ruby1_8?
  @@templates << 'import_charset_1_8' if Sass::Util.ruby1_8?

  @@cache_store = Sass::CacheStores::Memory.new

  def setup
    FileUtils.mkdir_p tempfile_loc
    FileUtils.mkdir_p tempfile_loc(nil,"more_")
    set_plugin_opts
    check_for_updates!
    reset_mtimes
  end

  def teardown
    clean_up_sassc
    Sass::Plugin.reset!
    FileUtils.rm_r tempfile_loc
    FileUtils.rm_r tempfile_loc(nil,"more_")
  end

  @@templates.each do |name|
    define_method("test_template_renders_correctly (#{name})") do
      assert_renders_correctly(name)
    end
  end

  def test_no_update
    File.delete(tempfile_loc('basic'))
    assert_needs_update 'basic'
    check_for_updates!
    assert_stylesheet_updated 'basic'
  end

  def test_update_needed_when_modified
    touch 'basic'
    assert_needs_update 'basic'
    check_for_updates!
    assert_stylesheet_updated 'basic'
  end

  def test_update_needed_when_dependency_modified
    touch 'basic'
    assert_needs_update 'import'
    check_for_updates!
    assert_stylesheet_updated 'basic'
    assert_stylesheet_updated 'import'
  end

  def test_update_needed_when_scss_dependency_modified
    touch 'scss_importee'
    assert_needs_update 'import'
    check_for_updates!
    assert_stylesheet_updated 'scss_importee'
    assert_stylesheet_updated 'import'
  end

  def test_scss_update_needed_when_dependency_modified
    touch 'basic'
    assert_needs_update 'scss_import'
    check_for_updates!
    assert_stylesheet_updated 'basic'
    assert_stylesheet_updated 'scss_import'
  end

  def test_update_needed_when_nested_import_dependency_modified
    touch 'basic'
    assert_needs_update 'nested_import'
    check_for_updates!
    assert_stylesheet_updated 'basic'
    assert_stylesheet_updated 'scss_import'
  end

  def test_no_updates_when_always_check_and_always_update_both_false
    Sass::Plugin.options[:always_update] = false
    Sass::Plugin.options[:always_check] = false

    touch 'basic'
    assert_needs_update 'basic'
    check_for_updates!

    # Check it's still stale
    assert_needs_update 'basic'
  end

  def test_full_exception_handling
    File.delete(tempfile_loc('bork1'))
    check_for_updates!
    File.open(tempfile_loc('bork1')) do |file|
      assert_equal(<<CSS.strip, file.read.split("\n")[0...6].join("\n"))
/*
Syntax error: Undefined variable: "$bork".
        on line 2 of #{template_loc('bork1')}

1: bork
2:   :bork $bork
CSS
    end
    File.delete(tempfile_loc('bork1'))
  end

  def test_full_exception_with_block_comment
    File.delete(tempfile_loc('bork5'))
    check_for_updates!
    File.open(tempfile_loc('bork5')) do |file|
      assert_equal(<<CSS.strip, file.read.split("\n")[0...7].join("\n"))
/*
Syntax error: Undefined variable: "$bork".
        on line 3 of #{template_loc('bork5')}

1: bork
2:   /* foo *\\/
3:   :bork $bork
CSS
    end
    File.delete(tempfile_loc('bork1'))
  end

  def test_single_level_import_loop
    File.delete(tempfile_loc('single_import_loop'))
    check_for_updates!
    File.open(tempfile_loc('single_import_loop')) do |file|
      assert_equal(<<CSS.strip, file.read.split("\n")[0...2].join("\n"))
/*
Syntax error: An @import loop has been found: #{template_loc('single_import_loop')} imports itself
CSS
    end
  end

  def test_double_level_import_loop
    File.delete(tempfile_loc('double_import_loop1'))
    check_for_updates!
    File.open(tempfile_loc('double_import_loop1')) do |file|
      assert_equal(<<CSS.strip, file.read.split("\n")[0...4].join("\n"))
/*
Syntax error: An @import loop has been found:
                  #{template_loc('double_import_loop1')} imports #{template_loc('_double_import_loop2')}
                  #{template_loc('_double_import_loop2')} imports #{template_loc('double_import_loop1')}
CSS
    end
  end

  def test_nonfull_exception_handling
    old_full_exception = Sass::Plugin.options[:full_exception]
    Sass::Plugin.options[:full_exception] = false

    File.delete(tempfile_loc('bork1'))
    assert_raise(Sass::SyntaxError) {check_for_updates!}
  ensure
    Sass::Plugin.options[:full_exception] = old_full_exception
  end

  def test_two_template_directories
    set_plugin_opts :template_location => {
      template_loc => tempfile_loc,
      template_loc(nil,'more_') => tempfile_loc(nil,'more_')
    }
    check_for_updates!
    ['more1', 'more_import'].each { |name| assert_renders_correctly(name, :prefix => 'more_') }
  end

  def test_two_template_directories_with_line_annotations
    set_plugin_opts :line_comments => true,
                    :style => :nested,
                    :template_location => {
                      template_loc => tempfile_loc,
                      template_loc(nil,'more_') => tempfile_loc(nil,'more_')
                    }
    check_for_updates!
    assert_renders_correctly('more1_with_line_comments', 'more1', :prefix => 'more_')
  end

  def test_doesnt_render_partials
    assert !File.exists?(tempfile_loc('_partial'))
  end

  def test_template_location_array
    assert_equal [[template_loc, tempfile_loc]], Sass::Plugin.template_location_array
  end

  def test_add_template_location
    Sass::Plugin.add_template_location(template_loc(nil, "more_"), tempfile_loc(nil, "more_"))
    assert_equal(
      [[template_loc, tempfile_loc], [template_loc(nil, "more_"), tempfile_loc(nil, "more_")]],
      Sass::Plugin.template_location_array)

    touch 'more1', 'more_'
    touch 'basic'
    assert_needs_update "more1", "more_"
    assert_needs_update "basic"
    check_for_updates!
    assert_doesnt_need_update "more1", "more_"
    assert_doesnt_need_update "basic"
  end

  def test_remove_template_location
    Sass::Plugin.add_template_location(template_loc(nil, "more_"), tempfile_loc(nil, "more_"))
    Sass::Plugin.remove_template_location(template_loc, tempfile_loc)
    assert_equal(
      [[template_loc(nil, "more_"), tempfile_loc(nil, "more_")]],
      Sass::Plugin.template_location_array)

    touch 'more1', 'more_'
    touch 'basic'
    assert_needs_update "more1", "more_"
    assert_needs_update "basic"
    check_for_updates!
    assert_doesnt_need_update "more1", "more_"
    assert_needs_update "basic"
  end

  def test_import_same_name
    assert_warning <<WARNING do
WARNING: In #{template_loc}:
  There are multiple files that match the name "same_name_different_partiality.scss":
    _same_name_different_partiality.scss
    same_name_different_partiality.scss
WARNING
      touch "_same_name_different_partiality"
      assert_needs_update "same_name_different_partiality"
    end
  end

  # Callbacks

  def test_updating_stylesheets_callback
    # Should run even when there's nothing to update
    Sass::Plugin.options[:template_location] = nil
    assert_callback :updating_stylesheets, []
  end

  def test_updating_stylesheets_callback_with_never_update
    Sass::Plugin.options[:never_update] = true
    assert_no_callback :updating_stylesheets
  end

  def test_updated_stylesheet_callback_for_updated_template
    Sass::Plugin.options[:always_update] = false
    touch 'basic'
    assert_no_callback :updated_stylesheet, template_loc("complex"), tempfile_loc("complex") do
      assert_callbacks(
        [:updated_stylesheet, template_loc("basic"), tempfile_loc("basic")],
        [:updated_stylesheet, template_loc("import"), tempfile_loc("import")])
    end
  end

  def test_updated_stylesheet_callback_for_fresh_template
    Sass::Plugin.options[:always_update] = false
    assert_no_callback :updated_stylesheet
  end

  def test_updated_stylesheet_callback_for_error_template
    Sass::Plugin.options[:always_update] = false
    touch 'bork1'
    assert_no_callback :updated_stylesheet
  end

  def test_not_updating_stylesheet_callback_for_fresh_template
    Sass::Plugin.options[:always_update] = false
    assert_callback :not_updating_stylesheet, template_loc("basic"), tempfile_loc("basic")
  end

  def test_not_updating_stylesheet_callback_for_updated_template
    Sass::Plugin.options[:always_update] = false
    assert_callback :not_updating_stylesheet, template_loc("complex"), tempfile_loc("complex") do
      assert_no_callbacks(
        [:updated_stylesheet, template_loc("basic"), tempfile_loc("basic")],
        [:updated_stylesheet, template_loc("import"), tempfile_loc("import")])
    end
  end

  def test_not_updating_stylesheet_callback_with_never_update
    Sass::Plugin.options[:never_update] = true
    assert_no_callback :not_updating_stylesheet
  end

  def test_not_updating_stylesheet_callback_for_partial
    Sass::Plugin.options[:always_update] = false
    assert_no_callback :not_updating_stylesheet, template_loc("_partial"), tempfile_loc("_partial")
  end

  def test_not_updating_stylesheet_callback_for_error
    Sass::Plugin.options[:always_update] = false
    touch 'bork1'
    assert_no_callback :not_updating_stylesheet, template_loc("bork1"), tempfile_loc("bork1")
  end

  def test_compilation_error_callback
    Sass::Plugin.options[:always_update] = false
    touch 'bork1'
    assert_callback(:compilation_error,
      lambda {|e| e.message == 'Undefined variable: "$bork".'},
      template_loc("bork1"), tempfile_loc("bork1"))
  end

  def test_compilation_error_callback_for_file_access
    Sass::Plugin.options[:always_update] = false
    assert_callback(:compilation_error,
      lambda {|e| e.is_a?(Errno::ENOENT)},
      template_loc("nonexistent"), tempfile_loc("nonexistent")) do
      Sass::Plugin.update_stylesheets([[template_loc("nonexistent"), tempfile_loc("nonexistent")]])
    end
  end

  def test_creating_directory_callback
    Sass::Plugin.options[:always_update] = false
    dir = File.join(tempfile_loc, "subdir", "nested_subdir")
    FileUtils.rm_r dir
    assert_callback :creating_directory, dir
  end

  ## Regression

  def test_cached_dependencies_update
    FileUtils.mv(template_loc("basic"), template_loc("basic", "more_"))
    set_plugin_opts :load_paths => [template_loc(nil, "more_")]

    touch 'basic', 'more_'
    assert_needs_update "import"
    check_for_updates!
    assert_renders_correctly("import")
  ensure
    FileUtils.mv(template_loc("basic", "more_"), template_loc("basic"))
  end

  def test_cached_relative_import
    old_always_update = Sass::Plugin.options[:always_update]
    Sass::Plugin.options[:always_update] = true
    check_for_updates!
    assert_renders_correctly('subdir/subdir')
  ensure
    Sass::Plugin.options[:always_update] = old_always_update
  end

  def test_cached_if
    set_plugin_opts :cache_store => Sass::CacheStores::Filesystem.new(tempfile_loc + '/cache')
    check_for_updates!
    assert_renders_correctly 'if'
    check_for_updates!
    assert_renders_correctly 'if'
  ensure
    set_plugin_opts
  end

  def test_cached_import_option
    set_plugin_opts :custom => {:whatever => "correct"}
    check_for_updates!
    assert_renders_correctly "cached_import_option"

    @@cache_store.reset!
    set_plugin_opts :custom => nil, :always_update => false
    check_for_updates!
    assert_renders_correctly "cached_import_option"

    set_plugin_opts :custom => {:whatever => "correct"}, :always_update => true
    check_for_updates!
    assert_renders_correctly "cached_import_option"
  ensure
    set_plugin_opts :custom => nil
  end

 private

  def assert_renders_correctly(*arguments)
    options = arguments.last.is_a?(Hash) ? arguments.pop : {}
    prefix = options[:prefix]
    result_name = arguments.shift
    tempfile_name = arguments.shift || result_name

    expected_str = File.read(result_loc(result_name, prefix))
    actual_str = File.read(tempfile_loc(tempfile_name, prefix))
    unless Sass::Util.ruby1_8?
      expected_str = expected_str.force_encoding('IBM866') if result_name == 'import_charset_ibm866'
      actual_str = actual_str.force_encoding('IBM866') if tempfile_name == 'import_charset_ibm866'
    end
    expected_lines = expected_str.split("\n")
    actual_lines = actual_str.split("\n")

    if actual_lines.first == "/*" && expected_lines.first != "/*"
      assert(false, actual_lines[0..Sass::Util.enum_with_index(actual_lines).find {|l, i| l == "*/"}.last].join("\n"))
    end

    expected_lines.zip(actual_lines).each_with_index do |pair, line|
      message = "template: #{result_name}\nline:     #{line + 1}"
      assert_equal(pair.first, pair.last, message)
    end
    if expected_lines.size < actual_lines.size
      assert(false, "#{actual_lines.size - expected_lines.size} Trailing lines found in #{tempfile_name}.css: #{actual_lines[expected_lines.size..-1].join('\n')}")
    end
  end

  def assert_stylesheet_updated(name)
    assert_doesnt_need_update name

    # Make sure it isn't an exception
    expected_lines = File.read(result_loc(name)).split("\n")
    actual_lines = File.read(tempfile_loc(name)).split("\n")
    if actual_lines.first == "/*" && expected_lines.first != "/*"
      assert(false, actual_lines[0..actual_lines.enum_with_index.find {|l, i| l == "*/"}.last].join("\n"))
    end
  end

  def assert_callback(name, *expected_args)
    run = false
    received_args = nil
    Sass::Plugin.send("on_#{name}") do |*args|
      received_args = args
      run ||= expected_args.zip(received_args).all? do |ea, ra|
        ea.respond_to?(:call) ? ea.call(ra) : ea == ra
      end
    end

    if block_given?
      Sass::Util.silence_sass_warnings {yield}
    else
      check_for_updates!
    end

    assert run, "Expected #{name} callback to be run with arguments:\n  #{expected_args.inspect}\nHowever, it got:\n  #{received_args.inspect}"
  end

  def assert_no_callback(name, *unexpected_args)
    Sass::Plugin.send("on_#{name}") do |*a|
      next unless unexpected_args.empty? || a == unexpected_args

      msg = "Expected #{name} callback not to be run"
      if !unexpected_args.empty?
        msg << " with arguments #{unexpected_args.inspect}"
      elsif !a.empty?
        msg << ",\n  was run with arguments #{a.inspect}"
      end

      flunk msg
    end

    if block_given?
      yield
    else
      check_for_updates!
    end
  end

  def assert_callbacks(*args)
    return check_for_updates! if args.empty?
    assert_callback(*args.pop) {assert_callbacks(*args)}
  end

  def assert_no_callbacks(*args)
    return check_for_updates! if args.empty?
    assert_no_callback(*args.pop) {assert_no_callbacks(*args)}
  end

  def check_for_updates!
    Sass::Util.silence_sass_warnings do
      Sass::Plugin.check_for_updates
    end
  end

  def assert_needs_update(*args)
    assert(Sass::Plugin::StalenessChecker.stylesheet_needs_update?(tempfile_loc(*args), template_loc(*args)),
      "Expected #{template_loc(*args)} to need an update.")
  end

  def assert_doesnt_need_update(*args)
    assert(!Sass::Plugin::StalenessChecker.stylesheet_needs_update?(tempfile_loc(*args), template_loc(*args)),
      "Expected #{template_loc(*args)} not to need an update.")
  end

  def touch(*args)
    FileUtils.touch(template_loc(*args))
  end

  def reset_mtimes
    Sass::Plugin::StalenessChecker.dependencies_cache = {}
    atime = Time.now
    mtime = Time.now - 5
    Dir["{#{template_loc},#{tempfile_loc}}/**/*.{css,sass,scss}"].each {|f| File.utime(atime, mtime, f)}
  end

  def template_loc(name = nil, prefix = nil)
    if name
      scss = absolutize "#{prefix}templates/#{name}.scss"
      File.exists?(scss) ? scss : absolutize("#{prefix}templates/#{name}.sass")
    else
      absolutize "#{prefix}templates"
    end
  end

  def tempfile_loc(name = nil, prefix = nil)
    if name
      absolutize "#{prefix}tmp/#{name}.css"
    else
      absolutize "#{prefix}tmp"
    end
  end

  def result_loc(name = nil, prefix = nil)
    if name
      absolutize "#{prefix}results/#{name}.css"
    else
      absolutize "#{prefix}results"
    end
  end

  def set_plugin_opts(overrides = {})
    Sass::Plugin.options.merge!(
      :template_location => template_loc,
      :css_location => tempfile_loc,
      :style => :compact,
      :always_update => true,
      :never_update => false,
      :full_exception => true,
      :cache_store => @@cache_store
    )
    Sass::Plugin.options.merge!(overrides)
  end
end

class Sass::Engine
  alias_method :old_render, :render

  def render
    raise "bork bork bork!" if @template[0] == "{bork now!}"
    old_render
  end
end
