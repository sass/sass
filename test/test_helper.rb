lib_dir = File.dirname(__FILE__) + '/../lib'

require 'test/unit'
require 'fileutils'
$:.unshift lib_dir unless $:.include?(lib_dir)
require 'sass'
require 'mathn' if ENV['MATHN'] == 'true'

Sass::RAILS_LOADED = true unless defined?(Sass::RAILS_LOADED)

Sass.tests_running = true

if defined?(Encoding)
  $-w, w = false, $-w
  Encoding.default_external = 'UTF-8'
  $-w = w
end

module Sass::Script::Functions
  def option(name)
    Sass::Script::Value::String.new(@options[name.value.to_sym].to_s)
  end
end

class Test::Unit::TestCase
  def munge_filename(opts = {})
    opts[:filename] ||= filename_for_test(opts[:syntax] || :sass)
    opts[:sourcemap_filename] ||= sourcemap_filename_for_test
    opts
  end

  def test_name
    caller.
      map {|c| Sass::Util.caller_info(c)[2]}.
      compact.
      map {|c| c.sub(/^(block|rescue) in /, '')}.
      find {|c| c =~ /^test_/}
  end

  def filename_for_test(syntax = :sass)
    "#{test_name}_inline.#{syntax}"
  end

  def sourcemap_filename_for_test(syntax = :sass)
    "#{test_name}_inline.css.map"
  end

  def clean_up_sassc
    path = File.dirname(__FILE__) + "/../.sass-cache"
    FileUtils.rm_r(path) if File.exist?(path)
  end

  def assert_warning(message)
    the_real_stderr, $stderr = $stderr, StringIO.new
    yield

    if message.is_a?(Regexp)
      assert_match message, $stderr.string.strip
    else
      assert_equal message.strip, $stderr.string.strip
    end
  ensure
    $stderr = the_real_stderr
  end

  def assert_no_warning
    the_real_stderr, $stderr = $stderr, StringIO.new
    yield

    assert_equal '', $stderr.string
  ensure
    $stderr = the_real_stderr
  end

  def silence_warnings(&block)
    Sass::Util.silence_warnings(&block)
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

module PublicApiLinter
  def lint_api(api_class, duck_type_class)
    define_method :test_lint_instance do
      assert lint_instance.is_a?(duck_type_class)
    end
    api_class.instance_methods.each do |meth|
      define_method :"test_has_#{meth}" do
        assert lint_instance.respond_to?(meth),
          "#{duck_type_class.name} does not implement #{meth}"
      end
    end
  end
end
