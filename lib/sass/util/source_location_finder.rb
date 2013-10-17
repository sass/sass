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
        substr = source_string[0...index]
        previous_newline_index = substr.rindex("\n") || 0
        index - previous_newline_index
      end
    end
  end
end
