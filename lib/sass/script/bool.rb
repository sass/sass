require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a boolean (true or false) value.
  class Bool < Literal
    # The Ruby value of the boolean.
    #
    # @return [Boolean]
    attr_reader :value
    alias_method :to_bool, :value

    # @return [String] "true" or "false"
    def to_s(opts = {})
      @value.to_s
    end
    alias_method :to_sass, :to_s
  end
end
