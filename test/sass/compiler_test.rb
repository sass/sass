#!/usr/bin/env ruby
require 'minitest/autorun'
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/plugin'
require 'sass/plugin/compiler'

require "listen/compat/test/session"

class CompilerTest < MiniTest::Test
  module MockWatcher
    attr_accessor :update_stylesheets_times
    attr_accessor :update_stylesheets_called_with
    attr_accessor :deleted_css_files

    def update_stylesheets(individual_files)
      @update_stylesheets_times ||= 0
      @update_stylesheets_times += 1
      (@update_stylesheets_called_with ||= []) << individual_files
    end

    def try_delete_css(css_filename)
      (@deleted_css_files ||= []) << css_filename
    end
  end

  def test_sass_callbacks_fire_from_listener_events
    c = watcher

    modified_fired = false
    c.on_template_modified do |sass_file|
      modified_fired = true
      assert_equal "changed.scss", sass_file
    end

    added_fired = false
    c.on_template_created do |sass_file|
      added_fired = true
      assert_equal "added.scss", sass_file
    end

    removed_fired = false
    c.on_template_deleted do |sass_file|
      removed_fired = true
      assert_equal "removed.scss", sass_file
    end

    files = %w(changed.scss added.scss removed.scss)
    events = [%w(changed.scss), %w(added.scss), %w(removed.scss)]

    session = _create_session {c.watch(files)}
    session.simulate_events(*events)
    session.interrupt

    assert modified_fired
    assert added_fired
    assert removed_fired
    assert_equal 2, c.update_stylesheets_times
  end

  def test_removing_a_sass_file_removes_corresponding_css_file
    c = watcher

    session = _create_session {c.watch([["foo"]])}
    session.simulate_events([], [], ["remove_me.scss"])
    session.interrupt

    assert_equal "./remove_me.css", c.deleted_css_files.first
  end

  def test_removing_a_individually_watched_sass_file_removes_corresponding_css_file
    c = watcher

    session = _create_session {c.watch([["foo"]])}
    session.simulate_events([], [], ["remove_me.scss"])
    session.interrupt

    assert_equal "./remove_me.css", c.deleted_css_files.first
  end

  def test_an_importer_can_watch_an_image
    image_importer = Sass::Importers::Filesystem.new(".")
    class << image_importer
      def watched_file?(filename)
        filename =~ /\.png$/
      end
    end
    c = watcher(:load_paths => [image_importer])

    modified_fired = false
    c.on_template_modified do |f|
      modified_fired = true
      assert_equal "image.png", f
    end

    session = _create_session {c.watch}
    session.simulate_events(["image.png"], [], [])
    session.interrupt

    assert modified_fired, "No modified event fired"
    assert_equal 2, c.update_stylesheets_times
  end

  def test_watching_specific_files_and_one_is_deleted
    c = watcher
    scss = _abs_path('foo.scss')
    css = _abs_path('foo.css')

    session = _create_session {c.watch([[scss, css, nil]])}
    session.simulate_events([], [], [scss])
    session.interrupt

    # TODO: separate test?
    # directories = listener_instances.first.directories
    # assert directories.include?(_abs_path(".")), directories.inspect

    expected = "the corresponding css file was not deleted"
    assert_equal css, c.deleted_css_files.first, expected

    expected = "the sass file should not have been compiled"
    assert_equal [], c.update_stylesheets_called_with[1], expected
  end

  def test_watched_directories_are_dedupped
    c = watcher(:load_paths => [".", "./foo", "."])

    session = _create_session {c.watch}
    session.interrupt

    directories = session.instances.first.directories

    assert_equal [_abs_path(".")], directories
  end

  def test_a_changed_css_in_a_watched_directory_does_not_force_a_compile
    c = watcher

    c.on_template_modified do |f|
      assert false, "Should not have been called"
    end

    session = _create_session {c.watch}
    session.simulate_events(["foo.css"], [], [])
    session.interrupt

    assert_equal 1, c.update_stylesheets_times
  end

  private

  def default_options
    {:template_location => [[".", "."]]}
  end

  def watcher(options = {})
    options = default_options.merge(options)
    watcher = Sass::Plugin::Compiler.new(options)
    watcher.extend(MockWatcher)
    watcher
  end

  def _create_session(&block)
    Listen::Compat::Test::Session.new(&block)
  end

  def _abs_path(path)
    File.expand_path(path)
  end
end
