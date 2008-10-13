require 'strscan'
require 'sass/script/variable'
require 'sass/script/funcall'
require 'sass/script/operation'
require 'sass/script/literal'
require 'sass/script/parser'

module Sass
  # This module contains various SassScript-related functionality.
  module Script
    # :stopdoc:
    # The character that begins a variable.
    VARIABLE_CHAR = ?!

    # The regular expression used to parse variables
    MATCH = /^!(\w+)\s*((?:\|\|)?=)\s*(.+)/

    # The regular expression used to validate variables without matching
    VALIDATE = /^!\w+$/

    def self.resolve(*args)
      parse(*args).to_s
    end

    def self.parse(value, environment, line)
      Parser.parse(value).perform(environment)
    rescue Sass::SyntaxError => e
      if e.message == "SassScript error"
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
