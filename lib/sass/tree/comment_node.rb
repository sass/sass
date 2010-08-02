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

    # @see Node#to_sass
    def to_sass(tabs, opts = {})
      content = value.gsub(/\*\/$/, '').rstrip
      if content =~ /\A[ \t]/
        # Re-indent SCSS comments like this:
        #     /* foo
        #   bar
        #       baz */
        content.gsub!(/^/, '   ')
        content.sub!(/\A([ \t]*)\/\*/, '/*\1')
      end

      content =
        unless content.include?("\n")
          content
        else
          content.gsub!(/\n( \*|\/\/)/, "\n  ")
          spaces = content.scan(/\n( *)/).map {|s| s.first.size}.min
          sep = silent ? "\n//" : "\n *"
          if spaces >= 2
            content.gsub(/\n  /, sep)
          else
            content.gsub(/\n#{' ' * spaces}/, sep)
          end
        end

      content.gsub!(/\A\/\*/, '//') if silent
      content.gsub!(/^/, '  ' * tabs)
      content.rstrip + "\n"
    end

    # @see Node#to_scss
    def to_scss(tabs, opts = {})
      spaces = ('  ' * [tabs - value[/^ */].size, 0].max)
      if silent
        value.gsub(/^[\/ ]\*/, '//').gsub(/ *\*\/$/, '')
      else
        value
      end.gsub(/^/, spaces) + "\n"
    end

    protected

    # Computes the CSS for the comment.
    #
    # Returns `nil` if this is a silent comment
    # or the current style doesn't render comments.
    #
    # @overload to_s(tabs = 0)
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String, nil] The resulting CSS
    # @see #invisible?
    def _to_s(tabs = 0, _ = nil)
      return if invisible?
      spaces = ('  ' * [tabs - 1 - value[/^ */].size, 0].max)

      content = value.gsub(/^/, spaces)
      content.gsub!(/\n +(\* *(?!\/))?/, ' ') if style == :compact
      content
    end

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
