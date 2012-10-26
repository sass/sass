module Sass::Source
  class Range
    # The starting position of the range in the document (inclusive).
    #
    # @return [Sass::Source::Position]
    attr_accessor :start_pos

    # The ending position of the range in the document (exclusive).
    #
    # @return [Sass::Source::Position]
    attr_accessor :end_pos

    # The file in which this source range appears, relative to the working
    # directory. This can be nil if the file is unknown or not yet generated.
    #
    # @return [String]
    attr_accessor :file

    # @param start_pos [Sass::Source::Position] See \{#start_pos}
    # @param end_pos [Sass::Source::Position] See \{#end_pos}
    # @param file [String] See \{#file}
    def initialize(start_pos, end_pos, file)
      @start_pos = start_pos
      @end_pos = end_pos
      @file = file
    end

    # @return [String] A string representation of the source range.
    def inspect
      "(#{start_pos.inspect} to #{end_pos.inspect}#{" in #{@file}" if @file})"
    end
  end
end
