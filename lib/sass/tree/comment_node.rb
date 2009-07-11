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

    # Whether or not the comment is silent (that is, doesn't output to CSS).
    #
    # @return [Boolean]
    attr_accessor :silent

    # @param value [String] See \{#value}
    # @param silent [Boolean] See \{#silent}
    def initialize(value, silent)
      @lines = []
      @value = value[2..-1].strip
      @silent = silent
      super()
    end

    # Compares the contents of two comments.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && value == other.value && silent == other.silent && lines == other.lines
    end

    # Computes the CSS for the comment.
    #
    # Returns `nil` if this is a silent comment
    # or the current style doesn't render comments.
    #
    # @overload to_s(tabs = 0)
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String, nil] The resulting CSS
    # @see #invisible?
    def to_s(tabs = 0, _ = nil)
      return if invisible?
      spaces = '  ' * (tabs - 1)

      content = (value.split("\n") + lines.map {|l| l.text})
      return spaces + "/* */" if content.empty?
      content.map! {|l| (l.empty? ? "" : " ") + l}
      content.first.gsub!(/^ /, '')
      content.last.gsub!(%r{ ?\*/ *$}, '')

      spaces + "/* " + content.join(style == :compact ? '' : "\n#{spaces} *") + " */"
    end

    # Returns `true` if this is a silent comment
    # or the current style doesn't render comments.
    #
    # @return [Boolean]
    def invisible?
      style == :compressed || @silent
    end

    protected

    # Removes this node from the tree if it's a silent comment.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @return [Tree::Node, Array<Tree::Node>] The resulting static nodes
    # @see Sass::Tree
    def _perform(environment)
      return [] if @silent
      self
    end
  end
end
