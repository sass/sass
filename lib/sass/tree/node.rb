module Sass
  module Tree
    class Node
      attr_reader :children
      attr_writer :children

      def initialize
        @children = []
      end

      def <<(child)
        @children << child
      end
      
      def to_s
        result = String.new
        children.each do |child|
          result += "#{child.to_s}\n"
        end
        result[0...-1]
      end
    end
  end
end
