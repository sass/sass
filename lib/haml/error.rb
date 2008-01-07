module Haml
  # The abstract type of exception raised by Haml code.
  class Error < StandardError; end

  # SyntaxError is the type of exception raised when Haml encounters an
  # ill-formatted document.
  # It's not particularly interesting, except in that it includes Haml::Error.
  class SyntaxError < Haml::Error; end

  # HamlError is the type of exception raised when Haml encounters an error
  # not of a syntactical nature, such as an undefined Filter.
  class HamlError < Haml::Error; end
end
