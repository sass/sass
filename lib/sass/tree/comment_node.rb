require 'sass/tree/node'

module Sass::Tree
  class CommentNode < ValueNode
    def initialize(value, style)
      super(value[2..-1].strip, style)
    end

    def to_s(tabs = 0, parent_name = nil)
      return if @style == :compressed

      spaces = '  ' * (tabs - 1)
      spaces + "/* " + ([value] + children.map {|c| c.text}).
        join(@style == :compact ? ' ' : "\n#{spaces} * ") + " */"
    end
  end
end
