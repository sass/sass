#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/engine'

class CacheTest < Test::Unit::TestCase
  @@cache_dir = "tmp/file_cache"

  def setup
    FileUtils.mkdir_p @@cache_dir
  end

  def teardown
    FileUtils.rm_rf @@cache_dir
  end

  def test_file_cache_writes_a_file
    file_store = Sass::CacheStores::Filesystem.new(@@cache_dir)
    file_store.store("asdf/foo.scssc", "fakesha1", root_node)
    assert File.exists?("#{@@cache_dir}/asdf/foo.scssc")
  end

  def test_file_cache_reads_a_file
    file_store = Sass::CacheStores::Filesystem.new(@@cache_dir)
    assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    file_store.store("asdf/foo.scssc", "fakesha1", root_node)
    assert File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    assert_kind_of Sass::Tree::RootNode, file_store.retrieve("asdf/foo.scssc", "fakesha1")
  end

  def test_file_cache_miss_returns_nil
    file_store = Sass::CacheStores::Filesystem.new(@@cache_dir)
    assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    assert_nil file_store.retrieve("asdf/foo.scssc", "fakesha1")
  end

  def test_sha_change_invalidates_cache_and_cleans_up
    file_store = Sass::CacheStores::Filesystem.new(@@cache_dir)
    assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    file_store.store("asdf/foo.scssc", "fakesha1", root_node)
    assert File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    assert_nil file_store.retrieve("asdf/foo.scssc", "differentsha1")
    assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
  end

  def test_version_change_invalidates_cache_and_cleans_up
    file_store = Sass::CacheStores::Filesystem.new(@@cache_dir)
    assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    file_store.store("asdf/foo.scssc", "fakesha1", root_node)
    assert File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    real_version = Sass::VERSION
    begin
      Sass::VERSION.replace("a different version")
      assert_nil file_store.retrieve("asdf/foo.scssc", "fakesha1")
      assert !File.exists?("#{@@cache_dir}/asdf/foo.scssc")
    ensure
      Sass::VERSION.replace(real_version)
    end
  end

  def test_arbitrary_objects_can_go_into_cache
    cache = Sass::CacheStores::Memory.new
    an_object = {:foo => :bar}
    cache.store("an_object", "", an_object)
    assert_equal an_object, cache.retrieve("an_object", "")
  end

  private
  def root_node
    Sass::Engine.new(<<-SCSS, :syntax => :scss).to_tree
      @mixin color($c) { color: $c}
      div { @include color(red); }
    SCSS
  end
end
