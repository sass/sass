

require File.dirname(__FILE__) + '/node'

module Sass
  module Css
    def self.parse(sass_file)
      build_tree(sass_file)
    end

    def self.build_tree(sass_tree)
      last_level, last_node = 0, nil
      base_container = []
      sass_tree.each do |line|
        line, level = whitespace_strip_and_count(line)
        #puts "#{level}:#{last_level} #{line}"
        if level == 0
          base_container << (last_node = Node.new(line))
        else
          if line[0] == ':'[0]
            if last_level == level
              (1 + last_level - level).times { last_node = last_node.parent }
              last_level = level
            end
            last_node.attributes << line
          else
            node = Node.new(line)
            if last_level <= level
              (1 + last_level - level).times { last_node = last_node.parent }
            end
            last_node << node
            last_level, last_node = level, node
          end
        end
      end
      return base_container
    end

    def self.whitespace_strip_and_count(line)
      [line.strip, line.scan(/[ ]*/)[0].length / 2]
    end
  end
end
