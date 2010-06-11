lib_dir = File.dirname(__FILE__) + '/../lib'
require File.dirname(__FILE__) + '/linked_rails'

require 'test/unit'
require 'fileutils'
$:.unshift lib_dir unless $:.include?(lib_dir)
require 'haml'
require 'sass'

require 'haml/template'
Haml::Template.options[:ugly] = false
Haml::Template.options[:format] = :xhtml

Sass::RAILS_LOADED = true unless defined?(Sass::RAILS_LOADED)

module Sass::Script::Functions
  module UserFunctions; end
  include UserFunctions

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
      map {|c| Haml::Util.caller_info(c)[2]}.
      compact.
      map {|c| c.sub(/^(block|rescue) in /, '')}.
      find {|c| c =~ /^test_/}
    "#{test_name}_inline.#{syntax}"
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

  def silence_warnings(&block)
    Haml::Util.silence_warnings(&block)
  end

  def rails_block_helper_char
    return '=' if Haml::Util.ap_geq_3?
    return '-'
  end

  def form_for_calling_convention(name)
    return "@#{name}, :as => :#{name}, :html => {:class => nil, :id => nil}" if Haml::Util.ap_geq_3?
    return ":#{name}, @#{name}"
  end
end
