module Sass::Tree
  class SourceRangeMapping < Struct.new(:from, :to, :source_filename, :id)
    def initialize(from, to, source_filename = nil)
      super
      @from = from;
      @to = to
      @source_filename = source_filename
    end

    def to_s
      "#{from} (#{source_filename if source_filename}) -> #{to}"
    end

    # @return [String] A string representation of the mapping
    def inspect(opts = {})
      to_s
    end
  end

  class SourceMapping
    include Sass::Util
    include Sass::Util::Base64VLQ

    # The mapping data ordered by the location in the target.
    #
    # @return [Array<Sass::Tree::SourceRangeMapping>]
    attr_reader :data

    def initialize
      @data = []
    end

    def append(other)
      @data.concat other.data
    end

    # Adds a new range mapping. The |to| range should be located after the one
    # in the previous invocation of this method (if any).
    #
    # @param from [Sass::Tree::SourceRange]
    # @param to [Sass::Tree::SourceRange]
    # @param source_filename [String]
    def add(from, to, source_filename)
      @data.push(SourceRangeMapping.new(from, to, source_filename))
    end

    def shift_to_ranges(line_delta, first_line_col_delta)
      @data.each do |m|
        m.to.start_pos.column += first_line_col_delta if m.to.start_pos.line == 0
        m.to.start_pos.line += line_delta if m.to.start_pos.line > 0
        m.to.end_pos.column += first_line_col_delta if m.to.end_pos.line == 0
        m.to.end_pos.line += line_delta if m.to.end_pos.line > 0
      end
    end

    def to_s
      @data.join("\n")
    end

    def to_json(target_filename, source_root = "", offset_position = nil)
      last_mapping = nil
      max_line = 0

      result = "{\n"
      write_json_field(result, "version", "3", true)

      source_filename_to_id = {}
      id_to_source_filename = {}
      filename_id = 0
      line_data = []
      segment_data_for_line = []

      # These track data necessary for the delta coding.
      previous_target_line = nil
      previous_target_column = 0
      previous_source_line = 0
      previous_source_column = 0
      previous_source_file_id = 0

      @data.each do |m|
        current_source_id = source_filename_to_id[m.source_filename]
        if !current_source_id
          source_filename_to_id[m.source_filename] = filename_id
          id_to_source_filename[filename_id] = m.source_filename
          current_source_id = filename_id
          filename_id += 1
        end

        adjusted_target_range = adjust_target_range(m.to, offset_position)
        [[m.from.start_pos, adjusted_target_range.start_pos], [m.from.end_pos, adjusted_target_range.end_pos]].each do |source_pos, target_pos|
          previous_target_column = 0 if target_pos.line != previous_target_line
          new_target_line = previous_target_line && previous_target_line != target_pos.line

          # |segment| is a data chunk for a single position mapping.
          segment = ""

          # Field 1: zero-based starting column.
          segment << encode_vlq(target_pos.column - previous_target_column)
          previous_target_column = target_pos.column

          # Field 2: zero-based index into the "sources" list.
          segment << encode_vlq(current_source_id - previous_source_file_id)
          previous_source_file_id = current_source_id

          # Field 3: zero-based starting line in the original source.
          segment << encode_vlq(source_pos.line - previous_source_line)
          previous_source_line = source_pos.line

          # Field 4: zero-based starting column in the original source.
          segment << encode_vlq(source_pos.column - previous_source_column)
          previous_source_column = source_pos.column

          for i in ((previous_target_line || 0)...target_pos.line)
            line_data.push("")
          end

          if new_target_line
            # Once the line changes, dump the segment data and prepare for the next line.
            line_data.push(segment_data_for_line.join(","))
            segment_data_for_line = [segment]
          else
            segment_data_for_line.push(segment)
          end

          previous_target_line = target_pos.line
        end
      end
      line_data.push(segment_data_for_line.join(","))
      write_json_field(result, "mappings", line_data.join(";"))

      source_names = []
      (0...filename_id).each {|id| source_names.push(id_to_source_filename[id].gsub(/\.\//, "") || "")}
      write_json_field(result, "sources", source_names)

      write_json_field(result, "sourceRoot", source_root)
      write_json_field(result, "file", target_filename)

      result << "\n}"
      result
    end

    private

    def write_json_field(out, name, value, is_first = false)
      out << (is_first ? "" : ",\n") << "\"" << json_escape_string(name) << "\": " << json_value_of(value)
    end

    def adjust_target_range(range, offset_position = nil)
      return range if !offset_position || (offset_position.line == 0 && offset_position.column == 0)
      start_pos = range.start_pos
      end_pos = range.end_pos
      offset_line = offset_position.line
      start_offset_column = offset_position.column
      end_offset_column = offset_position.column
      start_offset_column = 0 if start_pos.line > 0
      end_offset_column = 0 if end_pos.line > 0

      start_pos = SourcePosition.new(start_pos.line + offset_line, start_pos.column + start_offset_column)
      end_pos = SourcePosition.new(end_pos.line + offset_line, end_pos.column + end_offset_column)
      SourceRange.new(start_pos, end_pos)
    end
  end
end
