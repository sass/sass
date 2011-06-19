require 'sass/logger/log_level'

class Sass::Logger::Base
  
  include Sass::Logger::LogLevel

  attr_accessor :log_level
  attr_accessor :disabled

  log_level :trace
  log_level :debug
  log_level :info
  log_level :warn
  log_level :error

  def initialize(log_level = :debug)
    self.log_level = log_level
  end

  def logging_level?(level)
    !disabled && self.class.log_level?(level, log_level)
  end

  def log(level, message)
    self._log(level, message) if logging_level?(level)
  end

  def _log(level, message)
    Kernel::warn(message)
  end

end
