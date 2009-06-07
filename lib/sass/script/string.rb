require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a string of text.
  class String < Literal
    # The Ruby value of the string.
    #
    # @return [String]
    attr_reader :value
    alias_method :to_s, :value
  end
end
