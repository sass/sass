#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'

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
end
