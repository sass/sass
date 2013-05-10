#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/exec'
require 'tmpdir'

class ExecTest < Test::Unit::TestCase
  def setup
    @css_in = ".ruleset { margin: 0 }"
    @css_out = ".ruleset {\n  margin: 0;\n}\n"
    @sass_out = ".ruleset\n  margin: 0\n"
    @dir = Dir.mktmpdir
    @src = File.join(@dir, "src.scss")
    @dest = File.join(@dir, "dest.css")
    open(@src, 'wb') {|f| f.write(@css_in)}
  end

  def teardown
    FileUtils.rm_rf(@dir)
  end

  def to_options(commandline)
    return commandline.split(/\s+/).push(@src, @dest)
  end

  def binread(file)
    content = nil
    open(@dest, 'rb') {|f| content = f.read}
    return content
  end

  def test_exec
    Sass::Exec::Scss.new(to_options("-t expanded --unix-newlines")).parse
    assert_equal(@css_out, binread(@dest))
    Sass::Exec::SassConvert.new(to_options("-T sass --unix-newlines")).parse
    assert_equal(@sass_out, binread(@dest))
    Sass::Exec::SassConvert.new(to_options("-T sass --in-place --unix-newlines")).parse
    assert_equal(@sass_out, binread(@src))
  end

  def test_no_unix_newlines
    unless Sass::Util.windows?
      skip "Can be run on Windows only" if respond_to?(:skip)
      return
    end
    @css_out.gsub!(/\n/, "\r\n")
    @sass_out.gsub!(/\n/, "\r\n")
    Sass::Exec::Scss.new(to_options("-t expanded")).parse
    assert_equal(@css_out, binread(@dest))
    Sass::Exec::SassConvert.new(to_options("-T sass")).parse
    assert_equal(@sass_out, binread(@dest))
    Sass::Exec::SassConvert.new(to_options("-T sass --in-place")).parse
    assert_equal(@sass_out, binread(@src))
  end
end
