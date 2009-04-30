module Haml
  # An exception raised by Haml code.
  class Error < StandardError
    # The line of the template on which the error occurred.
    #
    # @return [Fixnum]
    attr_reader :line

    # @param message [String] The error message
    # @param line [Fixnum] See \{#line}
    def initialize(message = nil, line = nil)
      super(message)
      @line = line
    end
  end

  # SyntaxError is the type of exception raised when Haml encounters an
  # ill-formatted document.
  # It's not particularly interesting,
  # except in that it's a subclass of {Haml::Error}.
  class SyntaxError < Haml::Error; end
end
