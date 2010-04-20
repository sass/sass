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
    # The regular expression used to parse variables.
    MATCH = /^[!\$](#{Sass::SCSS::RX::IDENT})\s*((?:\|\|)?=|:)\s*(.+?)(!(?i:default))?$/

    # The regular expression used to validate variables without matching.
    VALIDATE = /^[!\$]#{Sass::SCSS::RX::IDENT}$/

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

    # @private
    def self.var_warning(varname, line, offset, filename)
      Haml::Util.haml_warn <<MESSAGE
DEPRECATION WARNING:
On line #{line}, character #{offset}#{" of '#{filename}'" if filename}
Variables with ! have been deprecated and will be removed in version 3.2.
Use \"$#{varname}\" instead.

You can use `sass-convert --in-place --from sass2 file.sass' to convert files automatically.
MESSAGE
    end

    # @private
    def self.equals_warning(types, name, val, guarded, line, offset, filename)
      Haml::Util.haml_warn <<MESSAGE
DEPRECATION WARNING:
On line #{line}#{", character #{offset}" if offset}#{" of '#{filename}'" if filename}
Setting #{types} with #{"||" if guarded}= has been deprecated and will be removed in version 3.2.
Use "#{name}: #{val}#{" !default" if guarded}" instead.

You can use `sass-convert --in-place --from sass2 file.sass' to convert files automatically.
MESSAGE
    end
  end
end
