require 'sass/script/node'
require 'sass/script/variable'
require 'sass/script/funcall'
require 'sass/script/operation'
require 'sass/script/literal'
require 'sass/script/parser'

module Sass
  # SassScript is code that's embedded in Sass documents
  # to allow for property values to be computed from variables.
  #
  # This module contains code that handles the parsing and evaluation of SassScript.
  module Script
    # The regular expression used to parse variables.
    MATCH = /^\$(#{Sass::SCSS::RX::IDENT})\s*:\s*(.+?)(!(?i:default))?$/

    # The regular expression used to validate variables without matching.
    VALIDATE = /^\$#{Sass::SCSS::RX::IDENT}$/

    # Parses a string of SassScript
    #
    # @param value [String] The SassScript
    # @param line [Fixnum] The number of the line on which the SassScript appeared.
    #   Used for error reporting
    # @param offset [Fixnum] The number of characters in on `line` that the SassScript started.
    #   Used for error reporting
    # @param options [{Symbol => Object}] An options hash;
    #   see {file:SASS_REFERENCE.md#sass_options the Sass options documentation}
    # @return [Script::Node] The root node of the parse tree
    def self.parse(value, line, offset, options = {})
      Parser.parse(value, line, offset, options)
    rescue Sass::SyntaxError => e
      e.message << ": #{value.inspect}." if e.message == "SassScript error"
      e.modify_backtrace(:line => line, :filename => options[:filename])
      raise e
    end

  end
end
