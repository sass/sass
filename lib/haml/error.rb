module Haml
  # An exception raised by Haml code.
  class Error < Exception
    # :stopdoc:

    # By default, an error is taken to refer to the line of the template
    # that was being processed when the exception was raised.
    # However, if line_offset is non-zero, it's added to that line number
    # to get the line to report for the error.
    attr_reader :line_offset

    def initialize(message = nil, line_offset = 0)
      super(message)
      @line_offset = line_offset
    end
    # :startdoc:
  end

  # SyntaxError is the type of exception raised when Haml encounters an
  # ill-formatted document.
  # It's not particularly interesting, except in that it includes Haml::Error.
  class SyntaxError < Haml::Error; end
end
