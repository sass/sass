require File.dirname(__FILE__) + '/attribute'

module Sass
  module Css
    class Node
      attr_reader :children, :attributes, :parent, :name
      attr_writer :children, :attributes, :parent, :name

      def initialize(name)
        @name = name
        @children, @attributes = [], []
      end

      def <<(child)
        @children << child
        child.parent = self
        true
      end

      def to_s
        name + "\n" + (self.children.collect { |c| c.to_s }).join("\n>>")
      end
    end
  end
end
