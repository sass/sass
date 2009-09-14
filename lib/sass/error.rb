module Sass
  # An exception class that keeps track of
  # the line of the Sass template it was raised on
  # and the Sass file that was being parsed (if applicable).
  #
  # All Sass errors are raised as {Sass::SyntaxError}s.
  #
  # When dealing with SyntaxErrors,
  # it's important to provide filename and line number information.
  # This will be used in various error reports to users, including backtraces;
  # see \{#sass\_backtrace} for details.
  #
  # Some of this information is usually provided as part of the constructor.
  # New backtrace entries can be added with \{#add\_backtrace},
  # which is called when an exception is raised between files (e.g. with `@import`).
  #
  # Often, a chunk of code will all have similar backtrace information -
  # the same filename or even line.
  # It may also be useful to have a default line number set.
  # In those situations, the default values can be used
  # by omitting the information on the original exception,
  # and then calling \{#modify\_backtrace} in a wrapper `rescue`.
  # When doing this, be sure that all exceptions ultimately end up
  # with the information filled in.
  class SyntaxError < StandardError
    # The backtrace of the error within Sass files.
    # This is an array of hashes containing information for a single entry.
    # The hashes have the following keys:
    #
    # `:filename`
    # : The name of the file in which the exception was raised,
    #   or `nil` if no filename is available.
    #
    # `:line`
    # : The line of the file on which the error occurred. Never nil.
    #
    # This information is also included in standard backtrace format
    # in the output of \{#backtrace}.
    #
    # @return [Aray<Hash<Symbol, Object>>]
    attr_accessor :sass_backtrace

    # The text of the template where this error was raised.
    #
    # @return [String]
    attr_accessor :sass_template

    # @param msg [String] The error message
    # @param attrs [Hash<Symbol, Object>] The information in the backtrace entry.
    #   See \{#sass\_backtrace}
    def initialize(msg, attrs = {})
      @message = msg
      @sass_backtrace = []
      add_backtrace(attrs)
    end

    # The name of the file in which the exception was raised.
    # This could be `nil` if no filename is available.
    #
    # @return [String]
    def sass_filename
      sass_backtrace.first[:filename]
    end

    # The line of the Sass template on which the error occurred.
    #
    # @return [Fixnum]
    def sass_line
      sass_backtrace.first[:line]
    end

    # Adds an entry to the exception's Sass backtrace.
    #
    # @param attrs [Hash<Symbol, Object>] The information in the backtrace entry.
    #   See \{#sass\_backtrace}
    def add_backtrace(attrs)
      sass_backtrace << attrs.reject {|k, v| v.nil?}
    end

    # Modify the top Sass backtrace entry (that is, the last one)
    # to have the given attributes.
    # If that entry already has one of the given attributes set,
    # that takes precendence.
    #
    # @param attrs [Hash<Symbol, Object>] The information to add to the backtrace entry.
    #   See \{#sass\_backtrace}
    def modify_backtrace(attrs)
      sass_backtrace[-1] = attrs.reject {|k, v| v.nil?}.merge(sass_backtrace.last)
    end

    # @return [String] The error message
    def to_s
      @message
    end

    # Returns the standard exception backtrace,
    # including the Sass backtrace.
    #
    # @return [Array<String>]
    def backtrace
      return nil if super.nil?
      sass_backtrace.map {|h| "#{h[:filename] || "(sass)"}:#{h[:line]}"} + super
    end

    # Returns a string representation of the Sass backtrace.
    #
    # @param default_filename [String] The filename to use for unknown files
    # @see #sass_backtrace
    # @return [String]
    def sass_backtrace_str(default_filename = "an unknown file")
      "Syntax error: #{message}" +
        Haml::Util.enum_with_index(sass_backtrace).map do |entry, i|
        "\n        #{i == 0 ? "on" : "from"} line #{entry[:line]}" +
          " of #{entry[:filename] || default_filename}"
      end.join
    end

    class << self
      # Returns an error report for an exception in CSS format.
      #
      # @param e [Exception]
      # @param full_exception [Boolean] The value of
      #   \{file:SASS\_REFERENCE.md#full_exception-option `options[:full_exception]`}
      def exception_to_css(e, options)
        return "/* Internal stylesheet error */" unless options[:full_exception]

        header = header_string(e, options)

        <<END
/*
#{header}

Backtrace:\n#{e.backtrace.join("\n")}
*/
body:before {
  white-space: pre;
  font-family: monospace;
  content: "#{header.gsub('"', '\"').gsub("\n", '\\A ')}"; }
END
      end

      private

      def header_string(e, options)
        return "#{e.class}: #{e.message}" unless e.is_a? Sass::SyntaxError

        line_offset = options[:line] || 1
        line_num = e.sass_line + 1 - line_offset
        min = [line_num - 6, 0].max
        section = e.sass_template.rstrip.split("\n")[min ... line_num + 5]
        return e.sass_backtrace_str if section.nil? || section.empty?

        e.sass_backtrace_str + "\n\n" + Haml::Util.enum_with_index(section).
          map {|line, i| "#{line_offset + min + i}: #{line}"}.join("\n")
      end
    end
  end

  # The class for Sass errors that are raised due to invalid unit conversions
  # in SassScript.
  class UnitConversionError < SyntaxError; end
end
