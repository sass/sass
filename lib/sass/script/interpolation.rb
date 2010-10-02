module Sass::Script
  # A SassScript object representing `#{}` interpolation outside a string.
  #
  # @see StringInterpolation
  class Interpolation < Node
    # Interpolation in a property is of the form `before #{mid} after`.
    #
    # @param before [Node] The SassScript before the interpolation
    # @param mid [Node] The SassScript within the interpolation
    # @param after [Node] The SassScript after the interpolation
    # @param wb [Boolean] Whether there was whitespace between `before` and `#{`
    # @param wa [Boolean] Whether there was whitespace between `}` and `after`
    # @param originally_text [Boolean]
    #   Whether the original format of the interpolation was plain text,
    #   not an interpolation.
    #   This is used when converting back to SassScript.
    def initialize(before, mid, after, wb, wa, originally_text = false)
      @before = before
      @mid = mid
      @after = after
      @whitespace_before = wb
      @whitespace_after = wa
      @originally_text = originally_text
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

    protected

    # Evaluates the interpolation.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Sass::Script::String] The SassScript string that is the value of the interpolation
    def _perform(environment)
      res = ""
      res << @before.perform(environment).to_s if @before
      res << " " if @before && @whitespace_before
      val = @mid.perform(environment)
      res << (val.is_a?(Sass::Script::String) ? val.value : val.to_s)
      res << " " if @after && @whitespace_after
      res << @after.perform(environment).to_s if @after
      opts(Sass::Script::String.new(res))
    end
  end
end
