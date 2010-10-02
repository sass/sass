module Sass::Script
  # A SassScript object representing `#{}` interpolation within a string.
  #
  # @see Interpolation
  class StringInterpolation < Node
    # Interpolation in a string is of the form `"before #{mid} after"`,
    # where `before` and `after` may include more interpolation.
    #
    # @param before [Node] The string before the interpolation
    # @param mid [Node] The SassScript within the interpolation
    # @param after [Node] The string after the interpolation
    def initialize(before, mid, after)
      @before = before
      @mid = mid
      @after = after
    end

    # @return [String] A human-readable s-expression representation of the interpolation
    def inspect
      "(string_interpolation #{@before.inspect} #{@mid.inspect} #{@after.inspect})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      # We can get rid of all of this when we remove the deprecated :equals context
      before_unquote, before_quote_char, before_str = parse_str(@before.to_sass(opts))
      after_unquote, after_quote_char, after_str = parse_str(@after.to_sass(opts))
      unquote = before_unquote || after_unquote ||
        (before_quote_char && !after_quote_char && !after_str.empty?) ||
        (!before_quote_char && after_quote_char && !before_str.empty?)
      quote_char =
        if before_quote_char && after_quote_char && before_quote_char != after_quote_char
          before_str.gsub!("\\'", "'")
          before_str.gsub!('"', "\\\"")
          after_str.gsub!("\\'", "'")
          after_str.gsub!('"', "\\\"")
          '"'
        else
          before_quote_char || after_quote_char
        end

      res = ""
      res << 'unquote(' if unquote
      res << quote_char if quote_char
      res << before_str
      res << '#{' << @mid.to_sass(opts) << '}'
      res << after_str
      res << quote_char if quote_char
      res << ')' if unquote
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
      before = @before.perform(environment)
      res << before.value
      mid = @mid.perform(environment)
      res << (mid.is_a?(Sass::Script::String) ? mid.value : mid.to_s)
      res << @after.perform(environment).value
      opts(Sass::Script::String.new(res, before.type))
    end

    private

    def parse_str(str)
      case str
      when /^unquote\((["'])(.*)\1\)$/
        return true, $1, $2
      when '""'
        return false, nil, ""
      when /^(["'])(.*)\1$/
        return false, $1, $2
      else
        return false, nil, str
      end
    end
  end
end
