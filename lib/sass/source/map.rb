module Sass::Source
  class Map
    # A mapping from one source range to another. Indicates that `input` was
    # compiled to `output`.
    #
    # @!attribute input
    #   @return [Sass::Source::Range] The source range in the input document.
    #
    # @!attribute output
    #   @return [Sass::Source::Range] The source range in the output document.
    #
    # @!attribute source_filename
    #   @return [String] The name of the input document.
    class Mapping < Struct.new(:input, :output, :source_filename)
      # @return [String] A string representation of the mapping.
      def inspect
        "#{input.inspect} => #{output.inspect}"
      end
    end

    # The mapping data ordered by the location in the target.
    #
    # @return [Array<Mapping>]
    attr_reader :data

    def initialize
      @data = []
    end

    # Adds a new mapping from one source range to another. Multiple invocations
    # of this method should have each `to` range come after all previous ranges.
    #
    # @param input [Sass::Source::Range]
    #   The source range in the input document.
    # @param output [Sass::Source::Range]
    #   The source range in the output document.
    # @param source_filename [String] The name of the input document.
    def add(input, output, source_filename)
      @data.push(Mapping.new(input, output, source_filename))
    end

    # Shifts all output source ranges forward one or more lines.
    #
    # @param delta [Fixnum] The number of lines to shift the ranges forward.
    def shift_output_lines(delta)
      return if delta == 0
      @data.each do |m|
        m.output.start_pos.line += delta
        m.output.end_pos.line += delta
      end
    end

    # Shifts any output source ranges that lie on the first line forward one or
    # more characters on that line.
    #
    # @param delta [Fixnum] The number of characters to shift the ranges
    #   forward.
    def shift_output_offsets(delta)
      return if delta == 0
      @data.each do |m|
        break if m.output.start_pos.line > 1
        m.output.start_pos.offset += delta
        m.output.end_pos.offset += delta if m.output.end_pos.line > 1
      end
    end

    # Returns the standard JSON representation of the source map.
    #
    # @param target_filename [String] The filename of the output file; that is,
    #   the target of the mapping.
    # @return [String] The JSON string.
    def to_json(target_filename)
      result = "{\n"
      write_json_field(result, "version", "3", true)

      source_filename_to_id = {}
      id_to_source_filename = {}
      next_source_id = 0
      line_data = []
      segment_data_for_line = []

      # These track data necessary for the delta coding.
      previous_target_line = nil
      previous_target_offset = 1
      previous_source_line = 1
      previous_source_offset = 1
      previous_source_id = 0

      @data.each do |m|
        current_source_id = source_filename_to_id[m.source_filename]
        unless current_source_id
          current_source_id = next_source_id
          next_source_id += 1

          source_filename_to_id[m.source_filename] = current_source_id
          id_to_source_filename[current_source_id] = m.source_filename
        end

        [
          [m.input.start_pos, m.output.start_pos],
          [m.input.end_pos, m.output.end_pos]
        ].each do |source_pos, target_pos|
          if previous_target_line != target_pos.line
            line_data.push(segment_data_for_line.join(",")) unless segment_data_for_line.empty?
            (target_pos.line - 1 - (previous_target_line || 0)).times {line_data.push("")}
            previous_target_line = target_pos.line
            previous_target_offset = 1
            segment_data_for_line = []
          end

          # `segment` is a data chunk for a single position mapping.
          segment = ""

          # Field 1: zero-based starting offset.
          segment << Sass::Util.encode_vlq(target_pos.offset - previous_target_offset)
          previous_target_offset = target_pos.offset

          # Field 2: zero-based index into the "sources" list.
          segment << Sass::Util.encode_vlq(current_source_id - previous_source_id)
          previous_source_id = current_source_id

          # Field 3: zero-based starting line in the original source.
          segment << Sass::Util.encode_vlq(source_pos.line - previous_source_line)
          previous_source_line = source_pos.line

          # Field 4: zero-based starting offset in the original source.
          segment << Sass::Util.encode_vlq(source_pos.offset - previous_source_offset)
          previous_source_offset = source_pos.offset

          segment_data_for_line.push(segment)

          previous_target_line = target_pos.line
        end
      end
      line_data.push(segment_data_for_line.join(","))
      write_json_field(result, "mappings", line_data.join(";"))

      source_names = []
      (0...next_source_id).each {|id| source_names.push(id_to_source_filename[id].gsub(/\.\//, "") || "")}
      write_json_field(result, "sources", source_names)
      write_json_field(result, "file", target_filename)

      result << "\n}"
      result
    end

    private

    def write_json_field(out, name, value, is_first = false)
      out << (is_first ? "" : ",\n") <<
        "\"" <<
        Sass::Util.json_escape_string(name) <<
        "\": " <<
        Sass::Util.json_value_of(value)
    end
  end
end
