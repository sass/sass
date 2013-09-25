#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'pathname'
require 'tmpdir'

class UtilTest < Test::Unit::TestCase
  include Sass::Util

  def test_scope
    assert(File.exist?(scope("Rakefile")))
  end

  def test_to_hash
    assert_equal({
        :foo => 1,
        :bar => 2,
        :baz => 3
      }, to_hash([[:foo, 1], [:bar, 2], [:baz, 3]]))
  end

  def test_map_keys
    assert_equal({
        "foo" => 1,
        "bar" => 2,
        "baz" => 3
      }, map_keys({:foo => 1, :bar => 2, :baz => 3}) {|k| k.to_s})
  end

  def test_map_vals
    assert_equal({
        :foo => "1",
        :bar => "2",
        :baz => "3"
      }, map_vals({:foo => 1, :bar => 2, :baz => 3}) {|k| k.to_s})
  end

  def test_map_hash
    assert_equal({
        "foo" => "1",
        "bar" => "2",
        "baz" => "3"
      }, map_hash({:foo => 1, :bar => 2, :baz => 3}) {|k, v| [k.to_s, v.to_s]})
  end

  def test_powerset
    return unless Set[Set[]] == Set[Set[]] # There's a bug in Ruby 1.8.6 that breaks nested set equality
    assert_equal([[].to_set].to_set,
      powerset([]))
    assert_equal([[].to_set, [1].to_set].to_set,
      powerset([1]))
    assert_equal([[].to_set, [1].to_set, [2].to_set, [1, 2].to_set].to_set,
      powerset([1, 2]))
    assert_equal([[].to_set, [1].to_set, [2].to_set, [3].to_set,
        [1, 2].to_set, [2, 3].to_set, [1, 3].to_set, [1, 2, 3].to_set].to_set,
      powerset([1, 2, 3]))
  end

  def test_restrict
    assert_equal(0.5, restrict(0.5, 0..1))
    assert_equal(1, restrict(2, 0..1))
    assert_equal(1.3, restrict(2, 0..1.3))
    assert_equal(0, restrict(-1, 0..1))
  end

  def test_merge_adjacent_strings
    assert_equal(["foo bar baz", :bang, "biz bop", 12],
      merge_adjacent_strings(["foo ", "bar ", "baz", :bang, "biz", " bop", 12]))
    str = "foo"
    assert_equal(["foo foo foo", :bang, "foo foo", 12],
      merge_adjacent_strings([str, " ", str, " ", str, :bang, str, " ", str, 12]))
  end

  def test_intersperse
    assert_equal(["foo", " ", "bar", " ", "baz"],
      intersperse(%w[foo bar baz], " "))
    assert_equal([], intersperse([], " "))
  end

  def test_substitute
    assert_equal(["foo", "bar", "baz", 3, 4],
      substitute([1, 2, 3, 4], [1, 2], ["foo", "bar", "baz"]))
    assert_equal([1, "foo", "bar", "baz", 4],
      substitute([1, 2, 3, 4], [2, 3], ["foo", "bar", "baz"]))
    assert_equal([1, 2, "foo", "bar", "baz"],
      substitute([1, 2, 3, 4], [3, 4], ["foo", "bar", "baz"]))

    assert_equal([1, "foo", "bar", "baz", 2, 3, 4],
      substitute([1, 2, 2, 2, 3, 4], [2, 2], ["foo", "bar", "baz"]))
  end

  def test_strip_string_array
    assert_equal(["foo ", " bar ", " baz"],
      strip_string_array([" foo ", " bar ", " baz "]))
    assert_equal([:foo, " bar ", " baz"],
      strip_string_array([:foo, " bar ", " baz "]))
    assert_equal(["foo ", " bar ", :baz],
      strip_string_array([" foo ", " bar ", :baz]))
  end

  def test_paths
    assert_equal([[1, 3, 5], [2, 3, 5], [1, 4, 5], [2, 4, 5]],
      paths([[1, 2], [3, 4], [5]]))
    assert_equal([[]], paths([]))
    assert_equal([[1, 2, 3]], paths([[1], [2], [3]]))
  end

  def test_lcs
    assert_equal([1, 2, 3], lcs([1, 2, 3], [1, 2, 3]))
    assert_equal([], lcs([], [1, 2, 3]))
    assert_equal([], lcs([1, 2, 3], []))
    assert_equal([1, 2, 3], lcs([5, 1, 4, 2, 3, 17], [0, 0, 1, 2, 6, 3]))

    assert_equal([1], lcs([1, 2, 3, 4], [4, 3, 2, 1]))
    assert_equal([1, 2], lcs([1, 2, 3, 4], [3, 4, 1, 2]))
  end

  def test_lcs_with_block
    assert_equal(["1", "2", "3"],
      lcs([1, 4, 2, 5, 3], [1, 2, 3]) {|a, b| a == b && a.to_s})
    assert_equal([-4, 2, 8],
      lcs([-5, 3, 2, 8], [-4, 1, 8]) {|a, b| (a - b).abs <= 1 && [a, b].max})
  end

  def test_group_by_to_a
    assert_equal([[1, [1, 3, 5, 7]], [0, [2, 4, 6, 8]]],
      group_by_to_a(1..8) {|i| i % 2})
    assert_equal([[1, [1, 4, 7, 10]], [2, [2, 5, 8, 11]], [0, [3, 6, 9, 12]]],
      group_by_to_a(1..12) {|i| i % 3})
  end

  def test_subsequence
    assert(subsequence?([1, 2, 3], [1, 2, 3]))
    assert(subsequence?([1, 2, 3], [1, :a, 2, :b, 3]))
    assert(subsequence?([1, 2, 3], [:a, 1, :b, :c, 2, :d, 3, :e, :f]))

    assert(!subsequence?([1, 2, 3], [1, 2]))
    assert(!subsequence?([1, 2, 3], [1, 3, 2]))
    assert(!subsequence?([1, 2, 3], [3, 2, 1]))
  end

  def test_silence_warnings
    old_stderr, $stderr = $stderr, StringIO.new
    warn "Out"
    assert_equal("Out\n", $stderr.string)
    silence_warnings {warn "In"}
    assert_equal("Out\n", $stderr.string)
  ensure
    $stderr = old_stderr
  end

  def test_sass_warn
    assert_warning("Foo!") {sass_warn "Foo!"}
  end

  def test_silence_sass_warnings
    old_stderr, $stderr = $stderr, StringIO.new
    silence_sass_warnings {warn "Out"}
    assert_equal("Out\n", $stderr.string)
    silence_sass_warnings {sass_warn "In"}
    assert_equal("Out\n", $stderr.string)
  ensure
    $stderr = old_stderr
  end

  def test_has
    assert(has?(:instance_method, String, :chomp!))
    assert(has?(:private_instance_method, Sass::Engine, :parse_interp))
  end

  def test_enum_with_index
    assert_equal(%w[foo0 bar1 baz2],
      enum_with_index(%w[foo bar baz]).map {|s, i| "#{s}#{i}"})
  end

  def test_enum_cons
    assert_equal(%w[foobar barbaz],
      enum_cons(%w[foo bar baz], 2).map {|s1, s2| "#{s1}#{s2}"})
  end

  def test_extract
    arr = [1, 2, 3, 4, 5]
    assert_equal([1, 3, 5], extract!(arr) {|e| e % 2 == 1})
    assert_equal([2, 4], arr)
  end

  def test_ord
    assert_equal(102, ord("f"))
    assert_equal(98, ord("bar"))
  end

  def test_flatten
    assert_equal([1, 2, 3], flatten([1, 2, 3], 0))
    assert_equal([1, 2, 3], flatten([1, 2, 3], 1))
    assert_equal([1, 2, 3], flatten([1, 2, 3], 2))

    assert_equal([[1, 2], 3], flatten([[1, 2], 3], 0))
    assert_equal([1, 2, 3], flatten([[1, 2], 3], 1))
    assert_equal([1, 2, 3], flatten([[1, 2], 3], 2))

    assert_equal([[[1], 2], [3], 4], flatten([[[1], 2], [3], 4], 0))
    assert_equal([[1], 2, 3, 4], flatten([[[1], 2], [3], 4], 1))
    assert_equal([1, 2, 3, 4], flatten([[[1], 2], [3], 4], 2))
  end

  def test_set_hash
    assert(set_hash(Set[1, 2, 3]) == set_hash(Set[3, 2, 1]))
    assert(set_hash(Set[1, 2, 3]) == set_hash(Set[1, 2, 3]))

    s1 = Set[]
    s1 << 1
    s1 << 2
    s1 << 3
    s2 = Set[]
    s2 << 3
    s2 << 2
    s2 << 1
    assert(set_hash(s1) == set_hash(s2))
  end

  def test_set_eql
    assert(set_eql?(Set[1, 2, 3], Set[3, 2, 1]))
    assert(set_eql?(Set[1, 2, 3], Set[1, 2, 3]))

    s1 = Set[]
    s1 << 1
    s1 << 2
    s1 << 3
    s2 = Set[]
    s2 << 3
    s2 << 2
    s2 << 1
    assert(set_eql?(s1, s2))
  end

  def test_extract_and_inject_values
    test = lambda {|arr| assert_equal(arr, with_extracted_values(arr) {|str| str})}

    test[['foo bar']]
    test[['foo {12} bar']]
    test[['foo {{12} bar']]
    test[['foo {{1', 12, '2} bar']]
    test[['foo 1', 2, '{3', 4, 5, 6, '{7}', 8]]
    test[['foo 1', [2, 3, 4], ' bar']]
    test[['foo ', 1, "\n bar\n", [2, 3, 4], "\n baz"]]
  end

  def nested_caller_info_fn
    caller_info
  end

  def double_nested_caller_info_fn
    nested_caller_info_fn
  end

  def test_caller_info
    assert_equal(["/tmp/foo.rb", 12, "fizzle"], caller_info("/tmp/foo.rb:12: in `fizzle'"))
    assert_equal(["/tmp/foo.rb", 12, nil], caller_info("/tmp/foo.rb:12"))
    assert_equal(["(sass)", 12, "blah"], caller_info("(sass):12: in `blah'"))
    assert_equal(["", 12, "boop"], caller_info(":12: in `boop'"))
    assert_equal(["/tmp/foo.rb", -12, "fizzle"], caller_info("/tmp/foo.rb:-12: in `fizzle'"))
    assert_equal(["/tmp/foo.rb", 12, "fizzle"], caller_info("/tmp/foo.rb:12: in `fizzle {}'"))

    info = nested_caller_info_fn
    assert_equal(__FILE__, info[0])
    assert_equal("test_caller_info", info[2])

    info = proc {nested_caller_info_fn}.call
    assert_equal(__FILE__, info[0])
    assert_match(/^(block in )?test_caller_info$/, info[2])

    info = double_nested_caller_info_fn
    assert_equal(__FILE__, info[0])
    assert_equal("double_nested_caller_info_fn", info[2])

    info = proc {double_nested_caller_info_fn}.call
    assert_equal(__FILE__, info[0])
    assert_equal("double_nested_caller_info_fn", info[2])
  end

  def test_version_gt
    assert_version_gt("2.0.0", "1.0.0")
    assert_version_gt("1.1.0", "1.0.0")
    assert_version_gt("1.0.1", "1.0.0")
    assert_version_gt("1.0.0", "1.0.0.rc")
    assert_version_gt("1.0.0.1", "1.0.0.rc")
    assert_version_gt("1.0.0.rc", "0.9.9")
    assert_version_gt("1.0.0.beta", "1.0.0.alpha")

    assert_version_eq("1.0.0", "1.0.0")
    assert_version_eq("1.0.0", "1.0.0.0")
  end

  def assert_version_gt(v1, v2)
    #assert(version_gt(v1, v2), "Expected #{v1} > #{v2}")
    assert(!version_gt(v2, v1), "Expected #{v2} < #{v1}")
  end

  def assert_version_eq(v1, v2)
    assert(!version_gt(v1, v2), "Expected #{v1} = #{v2}")
    assert(!version_gt(v2, v1), "Expected #{v2} = #{v1}")
  end

  class FooBar
    def foo
      Sass::Util.abstract(self)
    end
  end

  def test_abstract
    assert_raise_message(NotImplementedError,
      "UtilTest::FooBar must implement #foo") {FooBar.new.foo}
  end

  def test_atomic_writes
    # when using normal writes, this test fails about 90% of the time.
    filename = File.join(Dir.tmpdir, "test_atomic")
    5.times do
      writes_to_perform = %w(1 2 3 4 5 6 7 8 9).map {|i| "#{i}\n" * 100_000}
      threads = writes_to_perform.map do |to_write|
        Thread.new do
          # To see this test fail with a normal write,
          # change to the standard file open mechanism:
          # open(filename, "w") do |f|
          atomic_create_and_write_file(filename) do |f|
            f.write(to_write)
          end
        end
      end
      loop do
        contents = File.exist?(filename) ? File.read(filename) : nil
        next if contents.nil? || contents.size == 0
        unless writes_to_perform.include?(contents)
          if contents.size != writes_to_perform.first.size
            fail "Incomplete write detected: was #{contents.size} characters, " +
                 "should have been #{writes_to_perform.first.size}"
          else
            fail "Corrupted read/write detected"
          end
        end
        break if threads.all? {|t| !t.alive?}
      end
      threads.each {|t| t.join}
    end
  end

  class FakeError < RuntimeError; end

  def test_atomic_writes_handles_exceptions
    filename = File.join(Dir.tmpdir, "test_atomic_exception")
    FileUtils.rm_f(filename)
    tmp_filename = nil
    assert_raises FakeError do
      atomic_create_and_write_file(filename) do |f|
        tmp_filename = f.path
        raise FakeError.new "Borken"
      end
    end
    assert !File.exist?(filename)
    assert !File.exist?(tmp_filename)
  end
end
