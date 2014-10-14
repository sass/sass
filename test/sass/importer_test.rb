#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'mock_importer'
require 'sass/plugin'

class ImporterTest < MiniTest::Test

  class FruitImporter < Sass::Importers::Base
    def find(name, context = nil)
      fruit = parse(name)
      return unless fruit
      color = case fruit
      when "apple"
        "red"
      when "orange"
        "orange"
      else
        "blue"
      end
      contents = %Q{
        $#{fruit}-color: #{color} !default;
        @mixin #{fruit} {
          color: $#{fruit}-color;
        }
      }
      Sass::Engine.new(contents, :filename => name, :syntax => :scss, :importer => self)
    end

    def key(name, context)
      [self.class.name, name]
    end

    def public_url(name, sourcemap_directory = nil)
      "http://#{parse(name)}.example.com/style.scss"
    end

    private

    def parse(name)
      name[%r{fruits/(\w+)(\.s[ac]ss)?}, 1]
    end
  end

  class NoPublicUrlImporter < FruitImporter
    def public_url(name, sourcemap_directory = nil)
      nil
    end

    private

    def parse(name)
      name[%r{ephemeral/(\w+)(\.s[ac]ss)?}, 1]
    end
  end

  # This class proves that you can override the extension scheme for importers
  class ReversedExtImporter < Sass::Importers::Filesystem
    def extensions
      {"sscs" => :scss, "ssas" => :sass}
    end
  end

  # This importer maps one import to another import
  # based on the mappings passed to importer's constructor.
  class IndirectImporter < Sass::Importers::Base
    def initialize(mappings, mtimes)
      @mappings = mappings
      @mtimes = mtimes
    end
    def find_relative(uri, base, options)
      nil
    end
    def find(name, options)
      if @mappings.has_key?(name)
        Sass::Engine.new(
          %Q[@import "#{@mappings[name]}";],
          options.merge(
            :filename => name,
            :syntax => :scss,
            :importer => self
          )
        )
      end
    end
    def mtime(uri, options)
      @mtimes.fetch(uri, @mtimes.has_key?(uri) ? Time.now : nil)
    end
    def key(uri, options)
      [self.class.name, uri]
    end
    def to_s
      "IndirectImporter(#{@mappings.keys.join(", ")})"
    end
  end

  # This importer maps the import to single class
  # based on the mappings passed to importer's constructor.
  class ClassImporter < Sass::Importers::Base
    def initialize(mappings, mtimes)
      @mappings = mappings
      @mtimes = mtimes
    end
    def find_relative(uri, base, options)
      nil
    end
    def find(name, options)
      if @mappings.has_key?(name)
        Sass::Engine.new(
          %Q[.#{name}{#{@mappings[name]}}],
          options.merge(
            :filename => name,
            :syntax => :scss,
            :importer => self
          )
        )
      end
    end
    def mtime(uri, options)
      @mtimes.fetch(uri, @mtimes.has_key?(uri) ? Time.now : nil)
    end
    def key(uri, options)
      [self.class.name, uri]
    end
    def to_s
      "ClassImporter(#{@mappings.keys.join(", ")})"
    end
  end

  def test_can_resolve_generated_imports
    scss_file = %Q{
      $pear-color: green;
      @import "fruits/apple"; @import "fruits/orange"; @import "fruits/pear";
      .apple { @include apple; }
      .orange { @include orange; }
      .pear { @include pear; }
    }
    css_file = <<CSS
.apple { color: red; }

.orange { color: orange; }

.pear { color: green; }
CSS
    options = {:style => :compact, :load_paths => [FruitImporter.new], :syntax => :scss}
    assert_equal css_file, Sass::Engine.new(scss_file, options).render
  end

  def test_extension_overrides
    FileUtils.mkdir_p(absolutize("tmp"))
    open(absolutize("tmp/foo.ssas"), "w") {|f| f.write(".foo\n  reversed: true\n")}
    open(absolutize("tmp/bar.sscs"), "w") {|f| f.write(".bar {reversed: true}\n")}
    scss_file = %Q{
      @import "foo", "bar";
      @import "foo.ssas", "bar.sscs";
    }
    css_file = <<CSS
.foo { reversed: true; }

.bar { reversed: true; }

.foo { reversed: true; }

.bar { reversed: true; }
CSS
    options = {:style => :compact, :load_paths => [ReversedExtImporter.new(absolutize("tmp"))], :syntax => :scss}
    assert_equal css_file, Sass::Engine.new(scss_file, options).render
  ensure
    FileUtils.rm_rf(absolutize("tmp"))
  end

  def test_staleness_check_across_importers
    file_system_importer = Sass::Importers::Filesystem.new(fixture_dir)
    # Make sure the first import is older
    indirect_importer = IndirectImporter.new({"apple" => "pear"}, {"apple" => Time.now - 1})
    # Make css file is newer so the dependencies are the only way for the css file to be out of date.
    FileUtils.touch(fixture_file("test_staleness_check_across_importers.css"))
    # Make sure the first import is older
    class_importer = ClassImporter.new({"pear" => %Q{color: green;}}, {"pear" => Time.now + 1})

    options = {
      :style => :compact,
      :filename => fixture_file("test_staleness_check_across_importers.scss"),
      :importer => file_system_importer,
      :load_paths => [file_system_importer, indirect_importer, class_importer],
      :syntax => :scss
    }

    assert_equal File.read(fixture_file("test_staleness_check_across_importers.css")),
                 Sass::Engine.new(File.read(fixture_file("test_staleness_check_across_importers.scss")), options).render

    checker = Sass::Plugin::StalenessChecker.new(options)

    assert checker.stylesheet_needs_update?(
      fixture_file("test_staleness_check_across_importers.css"),
      fixture_file("test_staleness_check_across_importers.scss"),
      file_system_importer
    )
  end

  def test_source_map_with_only_css_uri_supports_public_url_imports
    fruit_importer = FruitImporter.new

    options = {
      :filename => 'fruits/orange',
      :importer => fruit_importer,
      :syntax => :scss
    }

    engine = Sass::Engine.new(<<SCSS, options)
.orchard {
  color: blue;
}
SCSS

    _, sourcemap = engine.render_with_sourcemap('sourcemap_uri')
    assert_equal <<JSON.strip, sourcemap.to_json(:css_uri => 'css_uri')
{
"version": 3,
"mappings": "AAAA,QAAS;EACP,KAAK,EAAE,IAAI",
"sources": ["http://orange.example.com/style.scss"],
"names": [],
"file": "css_uri"
}
JSON
  end

  def test_source_map_with_only_css_uri_can_have_no_public_url
    ephemeral_importer = NoPublicUrlImporter.new
    mock_importer = MockImporter.new
    def mock_importer.public_url(name, sourcemap_directory = nil)
      "source_uri"
    end

    options = {
      :filename => filename_for_test,
      :sourcemap_filename => sourcemap_filename_for_test,
      :importer => mock_importer,
      :syntax => :scss,
      :load_paths => [ephemeral_importer],
      :cache => false
    }

    engine = Sass::Engine.new(<<SCSS, options)
@import "ephemeral/orange";
.orange {
  @include orange;
}
SCSS

    css_output, sourcemap = engine.render_with_sourcemap('sourcemap_uri')
    assert_equal <<CSS.strip, css_output.strip
.orange {
  color: orange; }

/*# sourceMappingURL=sourcemap_uri */
CSS
    map = sourcemap.to_json(:css_uri => 'css_uri')
    assert_equal <<JSON.strip, map
{
"version": 3,
"mappings": "AACA,OAAQ",
"sources": ["source_uri"],
"names": [],
"file": "css_uri"
}
JSON
  end

  def test_source_map_with_only_css_uri_falls_back_to_file_uris
    file_system_importer = Sass::Importers::Filesystem.new('.')
    options = {
      :filename => filename_for_test(:scss),
      :sourcemap_filename => sourcemap_filename_for_test,
      :importer => file_system_importer,
      :syntax => :scss
    }

    engine = Sass::Engine.new(<<SCSS, options)
.foo {a: b}
SCSS

    _, sourcemap = engine.render_with_sourcemap('http://1.example.com/style.map')

    uri = Sass::Util.file_uri_from_path(Sass::Util.absolute_path(filename_for_test(:scss)))
    assert_equal <<JSON.strip, sourcemap.to_json(:css_uri => 'css_uri')
{
"version": 3,
"mappings": "AAAA,IAAK;EAAC,CAAC,EAAE,CAAC",
"sources": ["#{uri}"],
"names": [],
"file": "css_uri"
}
JSON
  end

  def test_source_map_with_css_uri_and_css_path_falls_back_to_file_uris
    file_system_importer = Sass::Importers::Filesystem.new('.')
    options = {
      :filename => filename_for_test(:scss),
      :sourcemap_filename => sourcemap_filename_for_test,
      :importer => file_system_importer,
      :syntax => :scss
    }

    engine = Sass::Engine.new(<<SCSS, options)
.foo {a: b}
SCSS

    _, sourcemap = engine.render_with_sourcemap('http://1.example.com/style.map')

    uri = Sass::Util.file_uri_from_path(Sass::Util.absolute_path(filename_for_test(:scss)))
    assert_equal <<JSON.strip, sourcemap.to_json(:css_uri => 'css_uri', :css_path => 'css_path')
{
"version": 3,
"mappings": "AAAA,IAAK;EAAC,CAAC,EAAE,CAAC",
"sources": ["#{uri}"],
"names": [],
"file": "css_uri"
}
JSON
  end

  def test_source_map_with_css_uri_and_sourcemap_path_supports_filesystem_importer
    file_system_importer = Sass::Importers::Filesystem.new('.')
    css_uri = 'css_uri'
    sourcemap_path = 'map/style.map'
    options = {
      :filename => 'sass/style.scss',
      :sourcemap_filename => sourcemap_path,
      :importer => file_system_importer,
      :syntax => :scss
    }

    engine = Sass::Engine.new(<<SCSS, options)
.foo {a: b}
SCSS

    rendered, sourcemap = engine.render_with_sourcemap('http://1.example.com/style.map')


    rendered, sourcemap = engine.render_with_sourcemap('http://map.example.com/map/style.map')
    assert_equal <<JSON.strip, sourcemap.to_json(:css_uri => css_uri, :sourcemap_path => sourcemap_path)
{
"version": 3,
"mappings": "AAAA,IAAK;EAAC,CAAC,EAAE,CAAC",
"sources": ["../sass/style.scss"],
"names": [],
"file": "css_uri"
}
JSON
  end

  def test_source_map_with_css_path_and_sourcemap_path_supports_file_system_importer
    file_system_importer = Sass::Importers::Filesystem.new('.')
    sass_path = 'sass/style.scss'
    css_path = 'static/style.css'
    sourcemap_path = 'map/style.map'
    options = {
      :filename => sass_path,
      :sourcemap_filename => sourcemap_path,
      :importer => file_system_importer,
      :syntax => :scss
    }

    engine = Sass::Engine.new(<<SCSS, options)
.foo {a: b}
SCSS

    _, sourcemap = engine.render_with_sourcemap('http://map.example.com/map/style.map')
    assert_equal <<JSON.strip, sourcemap.to_json(:css_path => css_path, :sourcemap_path => sourcemap_path)
{
"version": 3,
"mappings": "AAAA,IAAK;EAAC,CAAC,EAAE,CAAC",
"sources": ["../sass/style.scss"],
"names": [],
"file": "../static/style.css"
}
JSON
  end

  def test_render_with_sourcemap_requires_filename
    file_system_importer = Sass::Importers::Filesystem.new('.')
    engine = Sass::Engine.new(".foo {a: b}", :syntax => :scss, :importer => file_system_importer)
    assert_raise_message(Sass::SyntaxError, <<MESSAGE) {engine.render_with_sourcemap('sourcemap_url')}
Error generating source map: couldn't determine public URL for the source stylesheet.
  No filename is available so there's nothing for the source map to link to.
MESSAGE
  end

  def test_render_with_sourcemap_requires_importer_with_public_url
    class_importer = ClassImporter.new({"pear" => "color: green;"}, {"pear" => Time.now})
    assert_raise_message(Sass::SyntaxError, <<MESSAGE) {class_importer.find("pear", {}).render_with_sourcemap('sourcemap_url')}
Error generating source map: couldn't determine public URL for "pear".
  Without a public URL, there's nothing for the source map to link to.
  Custom importers should define the #public_url method.
MESSAGE
  end

  def fixture_dir
    File.join(File.dirname(__FILE__), "fixtures")
  end

  def fixture_file(path)
    File.join(fixture_dir, path)
  end

  def test_filesystem_importer_eql
    importer = Sass::Importers::Filesystem.new('.')
    assert importer.eql?(Sass::Importers::Filesystem.new('.'))
    assert importer.eql?(ReversedExtImporter.new('.'))
    assert !importer.eql?(Sass::Importers::Filesystem.new('foo'))
    assert !importer.eql?(nil)
    assert !importer.eql?('foo')
  end

  def test_absolute_files_across_template_locations
    importer = Sass::Importers::Filesystem.new(absolutize 'templates')
    refute_nil importer.mtime(absolutize('more_templates/more1.sass'), {})
  end
end
