module Sass::Script::Tree
  # A SassScript object representing `#{}` interpolation within a string.
  #
  # @see Interpolation
  class StringInterpolation < Node
    # @return [[Node, String]] The contents of the interpolation. Strings are
    #   literal text, nodes are interpolated segments.
    attr_reader :contents

    # Whether this is a CSS string or a CSS identifier. The difference is that
    # strings are written with double-quotes, while identifiers aren't.
    #
    # String interpolations are only ever identifiers if they're quote-like
    # functions such as `url()`.
    #
    # @return [Symbol] `:string` or `:identifier`
    attr_reader :type

    # Interpolation in a string.
    #
    # @param contents [[Node, String]] See {StringInterpolation#contents}
    # @param type [Symbol] See {StringInterpolation#type}
    def initialize(contents, type = :string)
      @contents = Sass::Util.merge_adjacent_strings(contents)
      @type = type
    end

    # @return [String] A human-readable s-expression representation of the interpolation
    def inspect
      "(string_interpolation #{@type.inspect} #{contents.map {|c| c.inspect}.join(', ')})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      quote = type == :string ? opts[:quote] || detected_quote : :none
      opts = opts.merge(:quote => quote)

      res = ""
      res << quote if quote != :none
      contents.each do |c|
        if c.is_a?(String)
          c = Sass::Script::Value::String.quote(c, opts).slice(1...-1) if quote != :none
          res << c
        else
          res << '#{' << c.to_sass(opts.merge(:quote => nil)) << '}'
        end
      end
      res << quote if quote != :none
      res
    end

    # Returns the node contents of this interpolation.
    #
    # @return [Array<Node>]
    # @see #initialize
    # @see Node#children
    def children
      contents.select {|c| c.is_a?(Sass::Script::Tree::Node)}
    end

    # @see Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@contents', contents.map do |c|
        c.is_a?(String) ? c : c.deep_copy
      end)
      node
    end

    protected

    # Evaluates the interpolation.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Sass::Script::Value::String]
    #   The SassScript string that is the value of the interpolation
    def _perform(environment)
      opts(Sass::Script::Value::String.new(contents.map do |c|
        next c if c.is_a?(String)
        value = c.perform(environment)
        value.is_a?(Sass::Script::Value::String) ? value.value : value.to_s(:quote => :none)
      end.join, type))
    end

    private

    # Returns the best quote to use for this interpolation.
    #
    # @return [String]
    def detected_quote
      has_double_quote = false
      contents.each do |c|
        next if c.is_a?(Sass::Script::Tree::Node)

        # Double-quotes take precedence over single quotes, so if any string contains
        # a single quote we'll definitely end up using double quotes.
        return '"' if c.include?("'")
        has_double_quote ||= c.include?('"')
      end

      has_double_quote ? "'" : '"'
    end
  end
end
