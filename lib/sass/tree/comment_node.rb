require 'sass/tree/node'

module Sass::Tree
  # A static node representing a Sass comment (silent or loud).
  #
  # @see Sass::Tree
  class CommentNode < Node
    # The text of the comment, not including `/*` and `*/`.
    # Interspersed with {Sass::Script::Node}s representing `#{}`-interpolation
    # if this is a loud comment.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :value

    # The text of the comment
    # after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_value

    # Whether the comment is loud.
    #
    # Loud comments start with ! and force the comment to be generated
    # irrespective of compilation settings or the comment syntax used.
    #
    # @return [Boolean]
    attr_accessor :loud

    # Whether or not the comment is silent (that is, doesn't output to CSS).
    #
    # @return [Boolean]
    attr_accessor :silent

    # @param value [Array<String, Sass::Script::Node>] See \{#value}
    # @param silent [Boolean] See \{#silent}
    # @param loud [Boolean] See \{#loud}
    def initialize(value, silent, loud)
      @value = Sass::Util.with_extracted_values(value) {|str| normalize_indentation str}
      @silent = silent
      @loud = loud
      super()
    end

    # Compares the contents of two comments.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && value == other.value && silent == other.silent && loud == other.loud
    end

    # Returns `true` if this is a silent comment
    # or the current style doesn't render comments.
    #
    # Comments starting with ! are never invisible (and the ! is removed from the output.)
    #
    # @return [Boolean]
    def invisible?
      if @loud
        return false
      else
        @silent || (style == :compressed)
      end
    end

    # Returns the number of lines in the comment.
    #
    # @return [Fixnum]
    def lines
      @value.inject(0) do |s, e|
        next s + e.count("\n") if e.is_a?(String)
        next s
      end
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
