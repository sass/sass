module Sass::Logger; end

require "sass/logger/log_level"
require "sass/logger/base"
require "sass/logger/delayed"

module Sass
  class << self
    attr_accessor :logger
  end

  self.logger = Sass::Logger::Base.new
end
