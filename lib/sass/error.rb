module Sass
  # An exception class that keeps track of
  # the line of the Sass template it was raised on
  # and the Sass file that was being parsed (if applicable).
  #
  # All Sass errors are raised as {Sass::SyntaxError}s.
  class SyntaxError < StandardError
    # The line of the Sass template on which the error occurred.
    #
    # @return [Fixnum]
    attr_accessor :sass_line

    # The name of the file that was being parsed when the exception was raised.
    # This could be `nil` if no filename is available.
    #
    # @return [String]
    attr_reader :sass_filename

    # @param msg [String] The error message
    # @param lineno [Fixnum] See \{#sass\_line}
    def initialize(msg, lineno = nil)
      @message = msg
      @sass_line = lineno
    end

    # Add information about the filename and line on which the error was raised,
    # and re-raises the exception.
    #
    # @param filename [String] See \{#sass\_filename}
    # @param line [Fixnum] See \{#sass\_line}
    # @raise [Sass::SyntaxError] self
    def add_metadata(filename, line)
      self.sass_line ||= line
      add_backtrace_entry(filename) unless sass_filename
      raise self
    end

    # Adds a properly formatted entry to the exception's backtrace.
    #
    # @param filename [String] The file in which the error occurred,
    #   if applicable (defaults to "(sass)")
    def add_backtrace_entry(filename) # :nodoc:
      @sass_filename ||= filename
      self.backtrace ||= []
      self.backtrace.unshift "#{@sass_filename || '(sass)'}:#{@sass_line}"
    end

    # @return [String] The error message
    def to_s
      @message
    end
  end

  # The class for Sass errors that are raised due to invalid unit conversions
  # in SassScript.
  class UnitConversionError < SyntaxError; end
end
