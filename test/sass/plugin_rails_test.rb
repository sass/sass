#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/plugin'

class PluginRailsTest < Test::Unit::TestCase

  def setup
    # Putting this here just to be a bit more explicit and clear about what's going on here.
    require 'sass/plugin/rails'
    assert Sass::RAILS_LOADED
  end

  # See plugin_configuration_test.
  def test_cache_store_option_allows_cache_location_option_to_be_set
    cache_location = 'somewhere'
    Sass::Plugin.options.merge!(:cache_location => cache_location)
    assert_equal cache_location, Sass::Plugin.options[:cache_store].cache_location
  end

end

