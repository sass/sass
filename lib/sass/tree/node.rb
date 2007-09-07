module Sass
  module Tree
    class Node
      attr_accessor :children
      attr_accessor :line
      attr_accessor :filename

      def initialize(style)
        @style = style
        @children = []
      end

      def <<(child)
        if msg = invalid_child?(child)
          raise Sass::SyntaxError.new(msg, child.line)
        end
        @children << child
      end
      
      def to_s
        result = String.new
        children.each do |child|
          if child.is_a? AttrNode
            raise SyntaxError.new('Attributes aren\'t allowed at the root of a document.', child.line)
          elsif child.is_a?(RuleNode) && child.continued?
            check_multiline_rule(child)
            result << child.to_s(1)
          else
            result << "#{child.to_s(1)}" + (@style == :compressed ? '' : "\n")
          end
        end
        @style == :compressed ? result+"\n" : result[0...-1]
      end

      protected

      def check_multiline_rule(rule)
        unless rule.children.empty?
          raise SyntaxError.new('Rules can\'t end in commas.', rule.line)
        end
      end

      private

      # This method should be overridden by subclasses to return an error message
      # if the given child node is invalid,
      # and false or nil otherwise.
      def invalid_child?(child)
        false
      end
    end
  end
end
