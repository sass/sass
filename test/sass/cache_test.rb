#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/engine'

class CacheTest < Test::Unit::TestCase
  @@cache_dir = "tmp/file_cache"

  def setup
    FileUtils.mkdir_p @@cache_dir
  end

  def teardown
    FileUtils.rm_rf @@cache_dir
    clean_up_sassc
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

  def test_cache_node_with_unmarshalable_option
    engine_with_unmarshalable_options("foo {a: b + c}").to_tree
  end

  # Regression tests

  def test_cache_mixin_def_splat_sass_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SASS, :syntax => :sass).to_tree
=color($args...)
  color: red
SASS
  end

  def test_cache_mixin_def_splat_scss_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SCSS, :syntax => :scss).to_tree
@mixin color($args...) {
  color: red;
}
SCSS
  end

  def test_cache_function_splat_sass_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SASS, :syntax => :sass).to_tree
@function color($args...)
  @return red
SASS
  end

  def test_cache_function_splat_scss_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SCSS, :syntax => :scss).to_tree
@function color($args...) {
  @return red;
}
SCSS
  end

  def test_cache_include_splat_sass_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SASS, :syntax => :sass).to_tree
@include color($args..., $kwargs...)
SASS
  end

  def test_cache_include_splat_scss_node_with_unmarshalable_option
    engine_with_unmarshalable_options(<<SCSS, :syntax => :scss).to_tree
@include color($args..., $kwargs...);
SCSS
  end

  private
  def root_node
    Sass::Engine.new(<<-SCSS, :syntax => :scss).to_tree
      @mixin color($c) { color: $c}
      div { @include color(red); }
    SCSS
  end

  def engine_with_unmarshalable_options(src, options={})
    Sass::Engine.new(src, {
      :syntax => :scss, :object => Class.new.new, :filename => 'file.scss',
      :importer => Sass::Importers::Filesystem.new(absolutize('templates'))
    }.merge(options))
  end
end
