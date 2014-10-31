module Sass::Script::Value
  # A SassScript object representing a null value.
  class Null < Base
    # The null value in SassScript.
    #
    # This is assigned before new is overridden below so that we use the default implementation.
    NULL = new(nil)

    # We override object creation so that users of the core API
    # will not need to know that null is a specific constant.
    #
    # @private
    # @return [Null] the {NULL} constant.
    def self.new
      NULL
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

    def single_eq(other)
      invalid_binary_op(:single_eq, other)
    end

    def plus(other)
      invalid_binary_op(:plus, other)
    end

    def minus(other)
      invalid_binary_op(:minus, other)
    end

    def div(other)
      invalid_binary_op(:div, other)
    end

    def times(other)
      invalid_binary_op(:times, other)
    end

    def to_sass(opts = {})
      'null'
    end

    # Returns a string representing a null value.
    #
    # @return [String]
    def inspect
      'null'
    end

    def to_sexp
      sass(:Script, :Value, :Null, :NULL)
    end

    private

    def invalid_binary_op(operator, other)
      raise Sass::SyntaxError.new("Invalid null operation: \"null #{operator} #{other.inspect}\".")
    end
  end
end
