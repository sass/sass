#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'

require 'sass/plugin'

class ImporterTest < Test::Unit::TestCase
  
  class FruitImporter < Sass::Importers::Base
    def find(name, context = nil)
      if name =~ %r{fruits/(\w+)(\.s[ac]ss)?}
        fruit = $1
        color = case $1
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
    end

    def key(name, context)
      [self.class.name, name]
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

  def fixture_dir
    File.join(File.dirname(__FILE__), "fixtures")
  end

  def fixture_file(path)
    File.join(fixture_dir, path)
  end

  def test_absolute_files_across_template_locations
    importer = Sass::Importers::Filesystem.new(absolutize 'templates')
    assert_not_nil importer.mtime(absolutize('more_templates/more1.sass'), {})
  end
end
