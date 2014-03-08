#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../../test_helper'

class SourceLocationFinderTest < Test::Unit::TestCase
  def test_column_at_index_before_start_of_string_is_zero
    @finder = Sass::Util::SourceLocationFinder.new("hello")
    assert_equal 0, @finder.column(0)
  end

  def test_column_starts_from_beginning_of_string_when_no_newlines
    @finder = Sass::Util::SourceLocationFinder.new("hello")
    assert_equal 3, @finder.column(3)
  end

  def test_column_starts_from_beginning_of_string_before_newlines
    @finder = Sass::Util::SourceLocationFinder.new("hello\nworld")
    assert_equal 3, @finder.column(3)
  end

  def test_column_starts_from_most_recent_newline
    @finder = Sass::Util::SourceLocationFinder.new("hello\nworld")
    assert_equal 4, @finder.column(9)
  end

  def test_column_on_a_newline_belongs_to_previous_line
    @finder = Sass::Util::SourceLocationFinder.new("hello\nworld")
    assert_equal 5, @finder.column(5)
  end

  def test_column_immediately_after_a_newline_belongs_to_next_line
    @finder = Sass::Util::SourceLocationFinder.new("hello\nworld")
    assert_equal 1, @finder.column(6)
  end
end
