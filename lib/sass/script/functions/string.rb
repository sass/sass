module Sass::Script::Functions

  # Removes quotes from a string if the string is quoted,
  # or returns the same string if it's not.
  #
  # @param str [String]
  # @return [String]
  # @raise [ArgumentError] if `str` isn't a string
  # @see #quote
  # @example
  #   unquote("foo") => foo
  #   unquote(foo) => foo
  def unquote(str)
    assert_type str, :String
    Sass::Script::String.new(str.value, :identifier)
  end

  # Add quotes to a string if the string isn't quoted,
  # or returns the same string if it is.
  #
  # @param str [String]
  # @return [String]
  # @raise [ArgumentError] if `str` isn't a string
  # @see #unquote
  # @example
  #   quote("foo") => "foo"
  #   quote(foo) => "foo"
  def quote(str)
    assert_type str, :String
    Sass::Script::String.new(str.value, :string)
  end

end
