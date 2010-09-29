module Sass::Script::Functions

  # The context in which methods in {Script::Functions} are evaluated.
  # That means that all instance methods of {EvaluationContext}
  # are available to use in functions.
  class EvaluationContext
    # The options hash for the {Sass::Engine} that is processing the function call
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # @param options [{Symbol => Object}] See \{#options}
    def initialize(options)
      @options = options

      # We need to include this individually in each instance
      # because of an icky Ruby restriction
      class << self; include Sass::Script::Functions; end
    end

    # Asserts that the type of a given SassScript value
    # is the expected type (designated by a symbol).
    #
    # Valid types are `:Bool`, `:Color`, `:Number`, and `:String`.
    # Note that `:String` will match both double-quoted strings
    # and unquoted identifiers.
    #
    # @example
    #   assert_type value, :String
    #   assert_type value, :Number
    # @param value [Sass::Script::Literal] A SassScript value
    # @param type [Symbol] The name of the type the value is expected to be
    def assert_type(value, type)
      return if value.is_a?(Sass::Script.const_get(type))
      raise ArgumentError.new("#{value.inspect} is not a #{type.to_s.downcase}")
    end
  end

end