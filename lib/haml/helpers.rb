module Haml
  module Helpers
    def flatten(input)
      input.gsub(/\n/, '&#x000A;').gsub(/\r/, '')
    end

    def find_and_flatten(input)
      input.scan(/<(textarea|code|pre)[^>]*>(.*?)<\/\1>/im).each do |thing|
        input = input.gsub(thing[1], flatten(thing[1]))
      end
      input
    end

    def tabs(count)
      '  ' * count
    end

    def count_soft_tabs(line)
      line.index(/[^ ]/) ? [line.index(/[^ ]/)/2, line.strip] : []
    end
  end
end
