require "sass/script/functions"

module Sass::Script::Functions

  # Removes quotes from a string if the string is quoted,
  # or returns the same string if it's not.
  #
  # @param string [String]
  # @return [String]
  # @raise [ArgumentError] if `string` isn't a string
  # @see #quote
  # @example
  #   unquote("foo") => foo
  #   unquote(foo) => foo
  def unquote(string)
    assert_type string, :String
    Sass::Script::String.new(string.value, :identifier)
  end
  define :unquote, :args => [:string]

  # Add quotes to a string if the string isn't quoted,
  # or returns the same string if it is.
  #
  # @param string [String]
  # @return [String]
  # @raise [ArgumentError] if `string` isn't a string
  # @see #unquote
  # @example
  #   quote("foo") => "foo"
  #   quote(foo) => "foo"
  def quote(string)
    assert_type string, :String
    Sass::Script::String.new(string.value, :string)
  end
  define :quote, :args => [:string]

end
