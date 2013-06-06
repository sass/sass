module Sass::Script::Value
  # A SassScript object representing a boolean (true or false) value.
  class Bool < Base

    TRUE  = self.new(true)
    FALSE = self.new(false)

    # We override object creation so that users of the core API
    # will not need to know that booleans are specific constants.
    #
    # @param value A ruby value that will be tested for truthiness.
    #
    # @return [Bool] TRUE if value is truthy, FALSE if value is falsey
    def self.new(value)
      value ? TRUE : FALSE
    end

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
