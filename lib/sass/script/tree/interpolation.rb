module Sass::Script::Tree
  # A SassScript object representing `#{}` interpolation outside a string.
  #
  # @see StringInterpolation
  class Interpolation < Node
    # @return [Node] The SassScript before the interpolation
    attr_reader :before

    # @return [Node] The SassScript within the interpolation
    attr_reader :mid

    # @return [Node] The SassScript after the interpolation
    attr_reader :after

    # @return [Boolean] Whether there was whitespace between `before` and `#{`
    attr_reader :whitespace_before

    # @return [Boolean] Whether there was whitespace between `}` and `after`
    attr_reader :whitespace_after

    # @return [Boolean] Whether the original format of the interpolation was
    #   plain text, not an interpolation. This is used when converting back to
    #   SassScript.
    attr_reader :originally_text

    # @return [Boolean] Whether a color value passed to the interpolation should
    #   generate a warning.
    attr_reader :warn_for_color

    # Interpolation in a property is of the form `before #{mid} after`.
    #
    # @param before [Node] See {Interpolation#before}
    # @param mid [Node] See {Interpolation#mid}
    # @param after [Node] See {Interpolation#after}
    # @param wb [Boolean] See {Interpolation#whitespace_before}
    # @param wa [Boolean] See {Interpolation#whitespace_after}
    # @param originally_text [Boolean] See {Interpolation#originally_text}
    # @param warn_for_color [Boolean] See {Interpolation#warn_for_color}
    # @comment
    #   rubocop:disable ParameterLists
    def initialize(before, mid, after, wb, wa, originally_text = false, warn_for_color = false)
      # rubocop:enable ParameterLists
      @before = before
      @mid = mid
      @after = after
      @whitespace_before = wb
      @whitespace_after = wa
      @originally_text = originally_text
      @warn_for_color = warn_for_color
    end

    # @return [String] A human-readable s-expression representation of the interpolation
    def inspect
      "(interpolation #{@before.inspect} #{@mid.inspect} #{@after.inspect})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      res = ""
      res << @before.to_sass(opts) if @before
      res << ' ' if @before && @whitespace_before
      res << '#{' unless @originally_text
      res << @mid.to_sass(opts)
      res << '}' unless @originally_text
      res << ' ' if @after && @whitespace_after
      res << @after.to_sass(opts) if @after
      res
    end

    # Returns the three components of the interpolation, `before`, `mid`, and `after`.
    #
    # @return [Array<Node>]
    # @see #initialize
    # @see Node#children
    def children
      [@before, @mid, @after].compact
    end

    # @see Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@before', @before.deep_copy) if @before
      node.instance_variable_set('@mid', @mid.deep_copy)
      node.instance_variable_set('@after', @after.deep_copy) if @after
      node
    end

    protected

    # Evaluates the interpolation.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Sass::Script::Value::String]
    #   The SassScript string that is the value of the interpolation
    def _perform(environment)
      res = ""
      res << @before.perform(environment).to_s if @before
      res << " " if @before && @whitespace_before

      val = @mid.perform(environment)
      if @warn_for_color && val.is_a?(Sass::Script::Value::Color) && val.name
        alternative = Operation.new(Sass::Script::Value::String.new("", :string), @mid, :plus)
        Sass::Util.sass_warn <<MESSAGE
WARNING on line #{line}, column #{source_range.start_pos.offset}#{" of #{filename}" if filename}:
You probably don't mean to use the color value `#{val}' in interpolation here.
It may end up represented as #{val.inspect}, which will likely produce invalid CSS.
Always quote color names when using them as strings (for example, "#{val}").
If you really want to use the color value here, use `#{alternative.to_sass}'.
MESSAGE
      end

      res << val.to_s(:quote => :none)
      res << " " if @after && @whitespace_after
      res << @after.perform(environment).to_s if @after
      opts(Sass::Script::Value::String.new(res))
    end
  end
end
