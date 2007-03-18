require 'sass/tree/node'

module Sass::Tree
  class CommentNode < ValueNode
    def initialize(value, style)
      super(value[2..-1].strip, style)
    end

    def to_s(parent_name = nil)
      join_string = @style == :compact ? ' ' : "\n * "
      "/* #{value}#{join_string unless children.empty?}#{children.join join_string} */"
    end
  end
end
