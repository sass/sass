module Sass
  module Tree
    class Node
      attr_accessor :children
      attr_accessor :line
      attr_accessor :filename

      def initialize(options)
        @options = options
        @style = options[:style]
        @children = []
      end

      def <<(child)
        if msg = invalid_child?(child)
          raise Sass::SyntaxError.new(msg, child.line)
        end
        @children << child
      end

      # We need this because Node duck types as an Array in engine.rb
      def last
        children.last
      end

      def to_s
        result = String.new
        children.each do |child|
          if child.is_a? AttrNode
            raise Sass::SyntaxError.new('Attributes aren\'t allowed at the root of a document.', child.line)
          else
            result << "#{child.to_s(1)}" + (@style == :compressed ? '' : "\n")
          end
        end
        @style == :compressed ? result+"\n" : result[0...-1]
      end

      def perform(environment)
        _perform(environment)
      rescue Sass::SyntaxError => e
        e.sass_line ||= line
        raise e
      end

      protected

      def _perform(environment)
        node = dup
        node.perform!(environment)
        node
      end

      def perform!(environment)
        self.children = perform_children(Environment.new(environment))
      end

      def perform_children(environment)
        children.map {|c| c.perform(environment)}.flatten
      end

      def interpolate(text, environment)
        scan = StringScanner.new(text)
        str = ''
        
        while scan.scan(/(.*?)(\\*)\#\{/)
          escapes = scan[2].size
          str << scan.matched[0...-2 - escapes]
          if escapes % 2 == 1
            str << '#{'
          else
            str << Sass::Script.resolve(balance(scan, ?{, ?}, 1)[0][0...-1], line, environment)
          end
        end
        
        str + scan.rest
      end
      
      def balance(*args)
        res = Haml::Shared.balance(*args)
        return res if res
        raise Sass::SyntaxError.new("Unbalanced brackets.", line)
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
