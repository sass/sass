#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require File.dirname(__FILE__) + '/test_helper'
require 'sass/plugin'

class PluginConfigurationTest < Test::Unit::TestCase

  # This test cannot be put in plugin_test.rb because the setup phase sets cache_store.
  # This needs to be tested without cache_store being set initially.
  def test_cache_store_option_allows_cache_location_option_to_be_set
    cache_location = 'somewhere'
    Sass::Plugin.options.merge!(:cache_location => cache_location)
    assert_equal cache_location, Sass::Plugin.options[:cache_store].cache_location
  end

end
