require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a null value.
  class Null < Literal
    # Creates a new null literal.
    def initialize
      super nil
    end

    # @return [Boolean] `false` (the Ruby boolean value)
    def to_bool
      false
    end

    # @return [Boolean] `true`
    def null?
      true
    end

    # @return [String] '' (An empty string)
    def to_s(opts = {})
      ''
    end
    alias_method :to_sass, :to_s

    # Returns a string representing a null value.
    #
    # @return [String]
    def inspect
      'null'
    end
  end
end
