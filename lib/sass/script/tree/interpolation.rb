module Sass::Script::Tree
  # A SassScript object representing a single `#{}` interpolation.
  #
  # This is used to wrap instances of interpolation in statement-level nodes. It
  # includes logic for converting back to the source representation, and it
  # optionally produces warnings when the values return named colors.
  #
  # TODO: Rather than representing this as a script node, it would be cleaner to
  # have a non-node Interpolation class that represents an interpolated chunk of
  # text.
  class Interpolation < Node
    # @return [Node] The SassScript within the
    attr_reader :value

    # @return [Boolean] Whether a color value passed to the interpolation should
    #   generate a warning.
    attr_reader :warn_for_color

    # @param value [Node] See {Interpolation#value}
    # @param warn_for_color [Boolean] See {Interpolation#warn_for_color}
    def initialize(value, warn_for_color = false)
      @value = value
      @warn_for_color = warn_for_color
    end

    # @return [String] A human-readable s-expression representation of the interpolation
    def inspect
      "(interpolation #{@value.inspect})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      "\#{#{@value.to_sass(opts)}}"
    end

    # Returns the three interpolation's value.
    #
    # @return [Array<Node>]
    # @see #initialize
    # @see Node#children
    def children
      [@value]
    end

    # @see Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@value', @value.deep_copy)
      node
    end

    protected

    # Evaluates the interpolation.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Sass::Script::Value::String]
    #   The SassScript string that is the value of the interpolation
    def _perform(environment)
      result = @value.perform(environment)
      if @warn_for_color && result.is_a?(Sass::Script::Value::Color) && result.name
        alternative = Operation.new(Sass::Script::Value::String.new("", :string), @value, :plus)
        Sass::Util.sass_warn <<MESSAGE
WARNING on line #{line}, column #{source_range.start_pos.offset}#{" of #{filename}" if filename}:
You probably don't mean to use the color value `#{result}' in interpolation here.
It may end up represented as #{result.inspect}, which will likely produce invalid CSS.
Always quote color names when using them as strings (for example, "#{result}").
If you really want to use the color value here, use `#{alternative.to_sass}'.
MESSAGE
      end

      opts(Sass::Script::Value::String.new(result.to_s(:quote => :none)))
    end
  end
end
