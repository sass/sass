require File.dirname(__FILE__) + '/../../test_helper'
require 'sass/engine'

module ScssTestHelper
  def assert_parses(scss)
    assert_equal scss.rstrip, render(scss).rstrip
  end

  def assert_not_parses(expected, scss)
    raise "Template must include <err> where an error is expected" unless scss.include?("<err>")

    after, was = scss.split("<err>")
    line = after.count("\n") + 1

    after.gsub!(/\s*\n\s*$/, '')
    after.gsub!(/.*\n/, '')
    after = "..." + after[-15..-1] if after.size > 18

    was.gsub!(/^\s*\n\s*/, '')
    was.gsub!(/\n.*/, '')
    was = was[0...15] + "..." if was.size > 18

    to_render = scss.sub("<err>", "")
    render(to_render)
    assert(false, "Expected syntax error for:\n#{to_render}\n")
  rescue Sass::SyntaxError => err
    assert_equal("Invalid CSS after \"#{after}\": expected #{expected}, was \"#{was}\"",
      err.message)
    assert_equal line, err.sass_line
  end

  def render(scss, options = {})
    options[:syntax] ||= :scss
    munge_filename options
    Sass::Engine.new(scss, options).render
  end
end
