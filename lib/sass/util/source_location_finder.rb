require 'strscan'

module Sass
  module Util
    # When processing a user-provided file, error messages should
    # include the causal line and column number. This computes the
    # column number given the file (as a string) and an offset into
    # the string.
    class SourceLocationFinder
      attr_reader :source_string

      def initialize(source_string)
        @source_string = source_string
      end

      def column(index)
        preceding_newlines = newline_positions.take_while {|pos| pos < index}
        # I think the length of this array is the line number
        previous_newline_index = preceding_newlines.last || 0
        index - previous_newline_index
      end

      private

      def newline_positions
        # Memoize this as walking through the string multiple times
        # might be very expensive.
        @newline_positions ||= build_newline_positions
      end

      def build_newline_positions
        scanner = StringScanner.new(source_string)
        newline_positions = []
        while scanner.scan_until(/\n/)
          start_position_of_newline = scanner.pos - scanner.matched_size
          newline_positions << start_position_of_newline
        end
        newline_positions
      end
    end
  end
end
