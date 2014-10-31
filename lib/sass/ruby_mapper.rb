module Sass
  class RubyMapper
    attr_reader :imports

    def initialize(ruby)
      @mapping = []
      ruby.split("\n").each_with_index do |line, i|
        next unless line =~ /^\s*#-s- (.*)/
        metadata = Sass::Util.to_hash($1.split(", ").map {|s| s.split(": ")}.map do |(k, v)|
          v = case v
              when 'nil'; nil
              when /^\d+/; v.to_i
              else; v
              end
          [k.to_sym, v]
        end)
        @mapping << [i + 1, metadata]
      end
      @mapping = Sass::Util.enum_with_index(@mapping).to_a
    end

    def find(line, *required_keys)
      result = Sass::Util.bsearch(@mapping) {|((l, _), _)| line <= l}
      i = result ? result.last - 1 : @mapping.length - 1
      metadata = @mapping[i][0][1]

      raise "[BUG] Couldn't find map entry for line #{line}" unless metadata

      until required_keys.all? {|k| metadata.has_key?(k)}
        raise "[BUG] Couldn't find map entry with keys #{required_keys.join(", ")}" if i < 0
        i -= 1
        metadata = @mapping[i][0][1].merge(metadata)
      end

      metadata
    end

    def frame_for(native_frame)
      metadata = find(native_frame[1], :name, :file, :type)
      type = metadata[:type]
      type = type ? type.to_sym : :base
      Sass::Stack::Frame.new(metadata[:file], metadata[:line], type, metadata[:name])
    end

    def stack_for(native_stack)
      stack = Sass::Stack.new
      last_line = nil
      native_stack.
          map {|frame| Sass::Util.caller_info(frame)}.
          select {|frame| frame[0] == '(eval)'}.reverse.
          each do |frame|
        next if frame[1] == last_line
        stack.frames << frame_for(frame)
        last_line = frame[1]
      end
      stack
    end
  end
end
