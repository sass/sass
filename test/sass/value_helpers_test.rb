#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'

class ValueHelpersTest < Test::Unit::TestCase
  include Sass::Script
  include Sass::Script::Value::Helpers

  def test_bool
    assert_same Value::Bool::TRUE, bool(true)
    assert_same Value::Bool::FALSE, bool(false)
    assert_same Value::Bool::FALSE, bool(nil)
    assert_same Value::Bool::TRUE, bool(Object.new)
  end

end
