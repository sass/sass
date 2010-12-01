require 'sass/tree/node'

module Sass::Tree
  # A static node representing a Sass comment (silent or loud).
  #
  # @see Sass::Tree
  class CommentNode < Node
    # The text of the comment, not including `/*` and `*/`.
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
      @value = normalize_indentation value
      @silent = silent
      super()
    end

    # Compares the contents of two comments.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && value == other.value && silent == other.silent
    end

    # Returns `true` if this is a silent comment
    # or the current style doesn't render comments.
    #
    # @return [Boolean]
    def invisible?
      style == :compressed || @silent
    end

    private

    def normalize_indentation(str)
      pre = str.split("\n").inject(str[/^[ \t]*/].split("")) do |pre, line|
        line[/^[ \t]*/].split("").zip(pre).inject([]) do |arr, (a, b)|
          break arr if a != b
          arr + [a]
        end
      end.join
      str.gsub(/^#{pre}/, '')
    end
  end
end
