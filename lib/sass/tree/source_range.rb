module Sass::Tree
  class SourceRange
    # The starting position of the range in the document (inclusive).
    #
    # @return [Sass::Tree::SourcePosition]
    attr_accessor :start_pos

    # The ending position of the range in the document (exclusive).
    #
    # @return [Sass::Tree::SourcePosition]
    attr_accessor :end_pos

    # @param startPos [Sass::Tree::SourcePosition] The starting position
    # @param endPos [Sass::Tree::SourcePosition] The ending position
    def initialize(start_pos, end_pos)
      @start_pos = start_pos
      @end_pos = end_pos
    end

    def to_s
      "[#{@start_pos}-#{@end_pos}]"
    end

    # @return [String] A string representation of the variable
    def inspect(opts = {})
      to_s
    end
  end
end
