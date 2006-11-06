#!/usr/bin/env ruby

require 'test/unit'
require File.dirname(__FILE__) + '/../lib/haml/helpers'

class HelperTest < Test::Unit::TestCase
  include Haml::Helpers

  def test_flatten
    assert_equal(flatten("FooBar"), "FooBar")

    assert_equal(flatten("Foo\rBar"), "FooBar")

    assert_equal(flatten("Foo\nBar"), "Foo&#x000A;Bar")

    assert_equal(flatten("Hello\nWorld!\nYOU ARE \rFLAT?\n\rOMGZ!"),
                         "Hello&#x000A;World!&#x000A;YOU ARE FLAT?&#x000A;OMGZ!")
  end

  def test_list_of_should_render_correctly
    assert_equal("<li>1</li>\n<li>2</li>", (list_of([1, 2]) { |i| i.to_s}))
    assert_equal("<li>1</li>", (list_of([[1]]) { |i| i.first}))
  end
end
