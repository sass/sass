#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'sass/util/test'
require 'tmpdir'

class ExecTest < MiniTest::Test
  include Sass::Util::Test

  def setup
    @dir = Dir.mktmpdir
  end

  def teardown
    FileUtils.rm_rf(@dir)
    clean_up_sassc
  end

  def test_scss_t_expanded
    src = get_path("src.scss")
    dest = get_path("dest.css")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[scss --sourcemap=none -t expanded --unix-newlines].push(src, dest)))
    assert_equal(".ruleset {\n  margin: 0;\n}\n", read(dest))
  end

  def test_sass_convert_T_sass
    src = get_path("src.scss")
    dest = get_path("dest.css")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[sass-convert -T sass --unix-newlines].push(src, dest)))
    assert_equal(".ruleset\n  margin: 0\n", read(dest))
  end

  def test_sass_convert_T_sass_in_place
    src = get_path("src.scss")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[sass-convert -T sass --in-place --unix-newlines].push(src)))
    assert_equal(".ruleset\n  margin: 0\n", read(src))
  end

  def test_scss_t_expanded_no_unix_newlines
    return skip "Can be run on Windows only" unless Sass::Util.windows?
    src = get_path("src.scss")
    dest = get_path("dest.css")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[scss -t expanded].push(src, dest)))
    assert_equal(".ruleset {\r\n  margin: 0;\r\n}\r\n", read(dest))
  end

  def test_sass_convert_T_sass_no_unix_newlines
    return skip "Can be run on Windows only" unless Sass::Util.windows?
    src = get_path("src.scss")
    dest = get_path("dest.sass")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[sass-convert -T sass].push(src, dest)))
    assert_equal(".ruleset\r\n  margin: 0\r\n", read(dest))
  end

  def test_sass_convert_T_sass_in_place_no_unix_newlines
    return skip "Can be run on Windows only" unless Sass::Util.windows?
    src = get_path("src.scss")
    write(src, ".ruleset { margin: 0 }")
    assert(exec(*%w[sass-convert -T sass --in-place].push(src)))
    assert_equal(".ruleset\r\n  margin: 0\r\n", read(src))
  end

  private

  def get_path(name)
    File.join(@dir, name)
  end

  def read(file)
    open(file, 'rb') {|f| f.read}
  end

  def write(file, content)
    open(file, 'wb') {|f| f.write(content)}
  end

  def exec(script, *args)
    script = File.dirname(__FILE__) + '/../../bin/' + script
    ruby = File.join(RbConfig::CONFIG['bindir'], RbConfig::CONFIG['ruby_install_name'] + RbConfig::CONFIG['EXEEXT'])
    system(ruby, script, *args)
  end
end
