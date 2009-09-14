require 'strscan'
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
    # The character that begins a variable.
    VARIABLE_CHAR = ?!

    # The regular expression used to parse variables.
    MATCH = /^!([a-zA-Z_]\w*)\s*((?:\|\|)?=)\s*(.+)/

    # The regular expression used to validate variables without matching.
    VALIDATE = /^![a-zA-Z_]\w*$/

    # Parses and evaluates a string of SassScript.
    #
    # @param value [String] The SassScript
    # @param line [Fixnum] The number of the line on which the SassScript appeared.
    #   Used for error reporting
    # @param offset [Fixnum] The number of characters in on `line` that the SassScript started.
    #   Used for error reporting
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [String] The string result of evaluating the SassScript
    def self.resolve(value, line, offset, environment)
      parse(value, line, offset).perform(environment).to_s
    end

    # Parses a string of SassScript
    #
    # @param value [String] The SassScript
    # @param line [Fixnum] The number of the line on which the SassScript appeared.
    #   Used for error reporting
    # @param offset [Fixnum] The number of characters in on `line` that the SassScript started.
    #   Used for error reporting
    # @param filename [String] The path to the file in which the SassScript appeared.
    #   Used for error reporting
    # @return [Script::Node] The root node of the parse tree
    def self.parse(value, line, offset, filename = nil)
      Parser.parse(value, line, offset, filename)
    rescue Sass::SyntaxError => e
      e.message << ": #{value.inspect}." if e.message == "SassScript error"
      e.modify_backtrace(:line => line, :filename => filename)
      raise e
    end
  end
end
