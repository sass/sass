require 'sass/tree/node'

module Sass::Tree
  class CommentNode < ValueNode
    def initialize(value, style)
      super(value[2..-1].strip, style)
    end

    def to_s(parent_name = nil)
      "/* #{value} */"
    end
  end
end
