module Sass::Tree
  class SourcePosition
    # The line of the document associated with the position (zero-based).
    #
    # @return [Fixnum]
    attr_accessor :line

    # The offset in the line of the document associated with the position
    # (zero-based).
    #
    # @return [Fixnum]
    attr_accessor :offset

    # @param line [Fixnum] The source line
    # @param offset [Fixnum] The source offset
    def initialize(line, offset)
      @line = line
      @offset = offset
    end

    # @return [String] A string representation of the source position.
    def inspect
      "#{line.inspect}:#{offset.inspect}"
    end
  end
end
