require 'sass/tree/node'

module Sass::Tree
  class CommentNode < ValueNode
    def initialize(value, style)
      super(value[2..-1].strip, style)
    end

    def to_s(tabs = 0, parent_name = nil)
      return if @style == :compressed

      spaces = '  ' * (tabs - 1)
      join_string = @style == :compact ? ' ' : "\n#{spaces} * "
      str = "#{spaces}/* #{value}"
      str << join_string unless children.empty?
      str << "#{children.join join_string} */"
      str
    end
  end
end
