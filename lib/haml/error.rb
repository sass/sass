module Haml
  # The abstract type of exception raised by Haml code.
  # Haml::SyntaxError includes this module,
  # as do all exceptions raised by Ruby code within Haml.
  #
  # Haml::Error encapsulates information about the exception,
  # such as the line of the Haml template it was raised on
  # and the Haml file that was being parsed (if applicable).
  # It also provides a handy way to rescue only exceptions raised
  # because of a faulty template.
  module Error
    # The line of the Haml template on which the exception was thrown.
    attr_reader :haml_line

    # The name of the file that was being parsed when the exception was raised.
    # This will be nil unless Haml is being used as an ActionView plugin.
    attr_reader :haml_filename

    # Adds a properly formatted entry to the exception's backtrace.
    # +lineno+ should be the line on which the error occurred.
    # +filename+ should be the file in which the error occurred,
    # if applicable (defaults to "(haml)").
    def add_backtrace_entry(lineno, filename = nil) # :nodoc:
      @haml_line = lineno
      @haml_filename = filename
      self.backtrace ||= []
      self.backtrace.unshift "#{filename || '(haml)'}:#{lineno}"
    end
  end

  # SyntaxError is the type of exception raised when Haml encounters an
  # ill-formatted document.
  # It's not particularly interesting, except in that it includes Haml::Error.
  class SyntaxError < StandardError
    include Haml::Error
  end

  # HamlError is the type of exception raised when Haml encounters an error
  # not of a syntactical nature, such as an undefined Filter.
  class HamlError < StandardError
    include Haml::Error
  end
end
