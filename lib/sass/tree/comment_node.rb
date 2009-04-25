require 'sass/tree/node'

module Sass::Tree
  # A static node representing a Sass comment (silent or loud).
  #
  # @see Sass::Tree
  class CommentNode < Node
    # The lines of text nested beneath the comment.
    #
    # @return [Array<Sass::Engine::Line>]
    attr_accessor :lines

    # The text on the same line as the comment starter.
    #
    # @return [String]
    attr_accessor :value

    # @param value [String] See \{#value}
    # @param options [Hash<Symbol, Object>] An options hash;
    #   see [the Sass options documentation](../../Sass.html#sass_options)
    def initialize(value, options)
      @lines = []
      @value = value[2..-1].strip
      super(options)
    end

    # Compares the contents of two comments.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && value == other.value && lines == other.lines
    end

    # @return [Boolean] Whether or not this is a silent comment
    def silent?
      !!@options[:silent]
    end

    # Computes the CSS for the comment.
    #
    # @call-seq to_s(tabs = 0)
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String] The resulting CSS
    def to_s(tabs = 0, _ = nil)
      return if (@style == :compressed || silent?)

      spaces = '  ' * (tabs - 1)
      spaces + "/* " + ([value] + lines.map {|l| l.text}).
        map{|l| l.sub(%r{ ?\*/ *$},'')}.join(@style == :compact ? ' ' : "\n#{spaces} * ") + " */"
    end
  end
end
