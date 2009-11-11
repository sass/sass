lib_dir = File.dirname(__FILE__) + '/../lib'
require File.dirname(__FILE__) + '/linked_rails'

require 'test/unit'
require 'fileutils'
$:.unshift lib_dir unless $:.include?(lib_dir)
require 'haml'
require 'sass'

Sass::RAILS_LOADED = true unless defined?(Sass::RAILS_LOADED)

class Test::Unit::TestCase
  def munge_filename(opts)
    return if opts[:filename]
    test_name = caller[1].gsub(/^.*`(?:\w+ )*(\w+)'.*$/, '\1')
    opts[:filename] = "#{test_name}_inline.sass"
  end

  def clean_up_sassc
    path = File.dirname(__FILE__) + "/../.sass-cache"
    FileUtils.rm_r(path) if File.exist?(path)
  end

  def assert_warning(message)
    the_real_stderr, $stderr = $stderr, StringIO.new
    yield
    assert_equal message.strip, $stderr.string.strip
  ensure
    $stderr = the_real_stderr
  end

  def silence_warnings(&block)
    Haml::Util.silence_warnings(&block)
  end
end
