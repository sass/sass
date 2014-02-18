lib_dir = File.dirname(__FILE__) + '/../lib'

require 'test/unit'
require 'fileutils'
$:.unshift lib_dir unless $:.include?(lib_dir)
require 'sass'
require 'json'
require 'mathn' if ENV['MATHN'] == 'true'

Sass::RAILS_LOADED = true unless defined?(Sass::RAILS_LOADED)
Encoding.default_external = 'UTF-8' if defined?(Encoding)

module Sass::Script::Functions
  def option(name)
    Sass::Script::String.new(@options[name.value.to_sym].to_s)
  end
end

class Test::Unit::TestCase
  def munge_filename(opts = {})
    return if opts.has_key?(:filename)
    opts[:filename] = filename_for_test(opts[:syntax] || :sass)
  end

  def filename_for_test(syntax = :sass)
    test_name = caller.
      map {|c| Sass::Util.caller_info(c)[2]}.
      compact.
      map {|c| c.sub(/^(block|rescue) in /, '')}.
      find {|c| c =~ /^test_/}
    "#{test_name}_inline.#{syntax}"
  end

  def clean_up_sassc
    path = File.dirname(__FILE__) + "/../.sass-cache"
    FileUtils.rm_r(path) if File.exist?(path)
  end

  def assert_warning(message, &block)
    stderr = collect_stderr(&block)
    if message.is_a?(Regexp)
      assert_match message, stderr.strip
    else
      assert_equal message.strip, stderr.strip
    end
  end

  def assert_no_warning(&block)
    assert_equal '', collect_stderr(&block)
  end

  def collect_stderr
    old_stderr, $stderr = $stderr, StringIO.new
    yield
    $stderr.string
  ensure
    $stderr = old_stderr
  end

  def silence_warnings(&block)
    Sass::Util.silence_warnings(&block)
  end

  def with_json_warnings
    old_json_err, Sass.json_err = Sass.json_err?, true
    yield
  ensure
    Sass.json_err = old_json_err
  end

  def collect_json_warnings(&block)
    with_json_warnings do
      JSON.parse(collect_stderr(&block))
    end
  end

  def assert_raise_message(klass, message)
    yield
  rescue Exception => e
    assert_instance_of(klass, e)
    assert_equal(message, e.message)
  else
    flunk "Expected exception #{klass}, none raised"
  end

  def assert_raise_line(line)
    yield
  rescue Sass::SyntaxError => e
    assert_equal(line, e.sass_line)
  else
    flunk "Expected exception on line #{line}, none raised"
  end
end
