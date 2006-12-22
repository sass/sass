require File.dirname(__FILE__) + '/../sass'
require 'sass/constant/string'

module Sass
  module Constant
    def self.parse(value, constants)
      constants.each do |key, const|
        value.gsub!("!#{key}", const.to_s)
      end
      Sass::Constant::String.new(value)
    end
  end
end
