require 'strscan'
require 'sass/constant/funcall'
require 'sass/constant/operation'
require 'sass/constant/literal'
require 'sass/constant/parser'

module Sass
  # This module contains various constant-script related functionality.
  module Constant
    # :stopdoc:
    # The character that begins a constant.
    CONSTANT_CHAR = ?!

    # The regular expression used to parse constants
    MATCH = /^!(\w+)\s*((?:\|\|)?=)\s*(.+)/

    # The regular expression used to validate constants without matching
    VALIDATE = /^!\w+$/

    def self.resolve(*args)
      parse(*args).to_s
    end

    def self.parse(value, constants, line)
      Parser.parse(value, constants).perform
    rescue Sass::SyntaxError => e
      if e.message == "Constant arithmetic error"
        e.instance_eval do
          @message += ": #{value.dump}."
        end
      end
      e.sass_line = line
      raise e
    end
    # :startdoc:
  end
end
