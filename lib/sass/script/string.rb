require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a string of text.
  class String < Literal
    # The Ruby value of the string.
    #
    # @return [String]
    attr_reader :value
    alias_method :to_s, :value

    # @see Node#to_sass
    def to_sass
      "\"#{value}\""
    end
  end
end
