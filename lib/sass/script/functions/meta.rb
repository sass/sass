module Sass::Script::Functions

  # Inspects the type of the argument, returning it as an unquoted string.
  #
  # @example
  #   type-of(100px)  => number
  #   type-of(asdf)   => string
  #   type-of("asdf") => string
  #   type-of(true)   => bool
  #   type-of(#fff)   => color
  #   type-of(blue)   => color
  # @param obj [Literal] The object to inspect
  # @return [String] The unquoted string name of the literal's type
  def type_of(obj)
    Sass::Script::String.new(obj.class.name.gsub(/Sass::Script::/,'').downcase)
  end

end
