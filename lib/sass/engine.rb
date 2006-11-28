require File.dirname(__FILE__) + '/css/parser'

module Sass
  class Engine
    def render_file(location)
      @result = ""
      File.open(location).each_line { |l| @result += l }
      render(@result)
    end
    
    def render(input)
      buffer, stack, last_level, first = "", [], 0, true
      input.each do |line|
        line, level = [line.strip, line.scan(/[ ]*/)[0].length / 2]
        unless line.empty?
          if '%.#'.index line[0..0]
            if !first
              buffer << "}\n"
            end
            if level <= last_level
              (last_level - level + 1).times do 
                stack.delete_at(stack.size - 1)
              end
              if stack.empty?
                buffer << "\n"
              end
            end
            stack << line
            buffer << stack.join(" ") + " { "
            last_level, first = level, false
          elsif ':' == line[0..0]
            attribute, *value = line.scan(/[^ ]*/)
            value = value.join(" ") if value.is_a? Array
            buffer << "#{attribute[1..-1]}: #{value.strip}; "
          end
          
        end
      end

      unless stack.empty?
        buffer << "}"
      end

      buffer
    end
  end
end
