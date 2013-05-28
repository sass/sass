module Sass::Script::Tree
  # The parse tree node for a literal scalar value. This wraps an instance of
  # {Sass::Script::Value::Base}.
  #
  # List literals should use {ListLiteral} instead.
  class Literal < Node
    # The wrapped value.
    #
    # @return [Sass::Script::Value::Base]
    attr_reader :value

    # Creates a new literal value.
    #
    # @param [Sass::Script::Value::Base] see {#value}
    def initialize(value)
      @value = value
    end

    # @see Node#children
    def children; []; end

    # @see Node#to_sass
    def to_sass(opts = {}); value.to_sass(opts); end

    # @see Node#deep_copy
    def deep_copy; dup; end

    # @see Node#options=
    def options=(options)
      value.options = options
    end

    def inspect
      value.inspect
    end

    protected

    def _perform(environment)
      value.line = line
      value.source_range = source_range
      value.filename = filename
      value
    end
  end
end
