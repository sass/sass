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

    # @return [String] A string representation of the source range.
    def inspect
      "(#{start_pos.inspect} to #{end_pos.inspect})"
    end
  end
end
