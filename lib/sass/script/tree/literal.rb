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
    # @param value [Sass::Script::Value::Base]
    # @see #value
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

    def _to_sexp(visitor)
      value_var = visitor.environment.unique_ident(:value)
      # TODO: Call actual constructors here.
      # TODO: Over the long term, stop associating source ranges with every value.
      s(:block,
        s(:lasgn, value_var, value.to_sexp),
        s(:attrasgn, s(:lvar, value_var), :source_range=, source_range.to_sexp),
        s(:lvar, value_var))
    end

    def _perform(environment)
      value.source_range = source_range
      value
    end
  end
end
