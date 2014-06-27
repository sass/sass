#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/../test_helper'
require 'pathname'

class LoggerTest < MiniTest::Test

  class InterceptedLogger < Sass::Logger::Base

    attr_accessor :messages

    def initialize(*args)
      super
      self.messages = []
    end

    def reset!
      self.messages = []
    end

    def _log(*args)
      messages << [args]
    end

  end

  def test_global_sass_logger_instance_exists
    assert Sass.logger.respond_to?(:warn)
  end

  def test_log_level_orders
    logged_levels = {
      :trace => [ [], [:trace, :debug, :info, :warn, :error]],
      :debug => [ [:trace],   [:debug, :info, :warn, :error]],
      :info  => [ [:trace, :debug],   [:info, :warn, :error]],
      :warn  => [ [:trace, :debug, :info],   [:warn, :error]],
      :error => [ [:trace, :debug, :info, :warn],   [:error]]
    }
    logged_levels.each do |level, (should_not_be_logged, should_be_logged)|
      logger = Sass::Logger::Base.new(level)
      should_not_be_logged.each do |should_level|
        assert !logger.logging_level?(should_level)
      end
      should_be_logged.each do |should_level|
        assert logger.logging_level?(should_level)
      end
    end
  end

  def test_logging_can_be_disabled
    logger = InterceptedLogger.new
    logger.error("message #1")
    assert_equal 1, logger.messages.size
    logger.reset!
    logger.disabled = true
    logger.error("message #2")
    assert_equal 0, logger.messages.size
  end
end
