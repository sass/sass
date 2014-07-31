#!/usr/bin/env ruby
require 'test/unit'
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/plugin'
require 'sass/plugin/compiler'

class CompilerTest < Test::Unit::TestCase
  class FakeListener
    attr_accessor :options
    attr_accessor :directories
    attr_reader :start_called
    attr_reader :thread

    def initialize(*args, &on_filesystem_event)
      self.options = args.last.is_a?(Hash) ? args.pop : {}
      self.directories = args
      @on_filesystem_event = on_filesystem_event
      @start_called = false
      reset_events!
    end

    def fire_events!(*args)
      @on_filesystem_event.call(@modified, @added, @removed)
      reset_events!
    end

    def changed(filename)
      @modified << File.expand_path(filename)
    end

    def added(filename)
      @added << File.expand_path(filename)
    end

    def removed(filename)
      @removed << File.expand_path(filename)
    end

    def on_start!(&run_during_start)
      @run_during_start = run_during_start
    end

    # used for Listen < 2.0
    def start!
      @run_during_start.call(self) if @run_during_start
    end

    # used for Listen >= 2.0
    def start
      @thread = Thread.new {@run_during_start.call(self) if @run_during_start}
    end

    def stop
    end

    def reset_events!
      @modified = []
      @added = []
      @removed = []
    end
  end

  module MockWatcher
    attr_accessor :run_during_start
    attr_accessor :update_stylesheets_times
    attr_accessor :update_stylesheets_called_with
    attr_accessor :deleted_css_files

    def fake_listener
      @fake_listener
    end

    def update_stylesheets(individual_files)
      @update_stylesheets_times ||= 0
      @update_stylesheets_times += 1
      (@update_stylesheets_called_with ||= []) << individual_files
    end

    def try_delete_css(css_filename)
      (@deleted_css_files ||= []) << css_filename
    end

    private
    def create_listener(*args, &on_filesystem_event)
      if Sass::Util.listen_geq_2?
        options = args.pop if args.last.is_a?(Hash)
        args.map do |dir|
          @fake_listener = FakeListener.new(*args, &on_filesystem_event)
          @fake_listener.on_start!(&run_during_start)
          @fake_listener
        end
      else
        @fake_listener = FakeListener.new(*args, &on_filesystem_event)
        @fake_listener.on_start!(&run_during_start)
        @fake_listener
      end
    end
  end

  def test_initialize
    watcher
  end

  def test_watch_starts_the_listener
    start_called = false
    c = watcher do |listener|
      start_called = true
    end
    c.watch
    assert start_called, "start! was not called"
  end

  def test_sass_callbacks_fire_from_listener_events
    c = watcher do |listener|
      listener.changed "changed.scss"
      listener.added "added.scss"
      listener.removed "removed.scss"
      listener.fire_events!
    end

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

    c.watch

    assert_equal 2, c.update_stylesheets_times
    assert modified_fired
    assert added_fired
    assert removed_fired
  end

  def test_removing_a_sass_file_removes_corresponding_css_file
    c = watcher do |listener|
      listener.removed "remove_me.scss"
      listener.fire_events!
    end

    c.watch

    assert_equal "./remove_me.css", c.deleted_css_files.first
  end

  def test_an_importer_can_watch_an_image
    image_importer = Sass::Importers::Filesystem.new(".")
    class << image_importer
      def watched_file?(filename)
        filename =~ /\.png$/
      end
    end
    c = watcher(:load_paths => [image_importer]) do |listener|
      listener.changed "image.png"
      listener.fire_events!
    end

    modified_fired = false
    c.on_template_modified do |f|
      modified_fired = true
      assert_equal "image.png", f
    end

    c.watch

    assert_equal 2, c.update_stylesheets_times
    assert modified_fired
  end

  def test_watching_specific_files_and_one_is_deleted
    directories = nil
    c = watcher do |listener|
      directories = listener.directories
      listener.removed File.expand_path("./foo.scss")
      listener.fire_events!
    end
    c.watch([[File.expand_path("./foo.scss"), File.expand_path("./foo.css"), nil]])
    assert directories.include?(File.expand_path(".")), directories.inspect
    assert_equal File.expand_path("./foo.css"), c.deleted_css_files.first, "the corresponding css file was not deleted"
    assert_equal [], c.update_stylesheets_called_with[1], "the sass file should not have been compiled"
  end

  def test_watched_directories_are_dedupped
    directories = nil
    c = watcher(:load_paths => [".", "./foo", "."]) do |listener|
      directories = listener.directories
    end
    c.watch
    assert_equal [File.expand_path(".")], directories
  end

  def test_a_changed_css_in_a_watched_directory_does_not_force_a_compile
    c = watcher do |listener|
      listener.changed "foo.css"
      listener.fire_events!
    end

    c.on_template_modified do |f|
      assert false, "Should not have been called"
    end

    c.watch

    assert_equal 1, c.update_stylesheets_times
  end

  private

  def default_options
    {:template_location => [[".","."]]}
  end

  def watcher(options = {}, &run_during_start)
    options = default_options.merge(options)
    watcher = Sass::Plugin::Compiler.new(options)
    watcher.extend(MockWatcher)
    watcher.run_during_start = run_during_start
    watcher
  end
end
