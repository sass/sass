require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a boolean (true or false) value.
  class Nil < Literal
    # The Ruby value of the boolean.
    #
    # @return [NilClass]
    attr_reader :value

    # Creates a new nil literal.
    def initialize
      super nil
    end

    # @return [Boolean] `false` (the Ruby boolean value)
    def to_bool
      false
    end

    # @return [Boolean] `true`
    def nil?
      true
    end

    # @return [String] '' (An empty string)
    def to_s(opts = {})
      @value.to_s
    end
    alias_method :to_sass, :to_s
  end
end
