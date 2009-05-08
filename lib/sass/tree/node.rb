module Sass
  module Tree
    class Node
      attr_accessor :children
      attr_accessor :line
      attr_writer :filename
      attr_reader :options

      def initialize
        @children = []
      end

      def options=(options)
        children.each {|c| c.options = options}
        @options = options
      end

      def filename
        @filename || @options[:filename]
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

      def ==(other)
        self.class == other.class && other.children == children
      end

      def render
        perform(Environment.new).to_s
      end

      def invisible?; false; end

      def to_s
        result = String.new
        children.each do |child|
          if child.is_a? AttrNode
            raise Sass::SyntaxError.new('Attributes aren\'t allowed at the root of a document.', child.line)
          else
            next if child.invisible?
            child_str = child.to_s(1)
            result << child_str + (style == :compressed ? '' : "\n")
          end
        end
        style == :compressed ? result+"\n" : result[0...-1]
      rescue Sass::SyntaxError => e; e.add_metadata(filename, line)
      end

      def perform(environment)
        environment.options = @options if self.class == Tree::Node
        _perform(environment)
      rescue Sass::SyntaxError => e; e.add_metadata(filename, line)
      end

      def style
        @options[:style]
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
        res = ''
        rest = Haml::Shared.handle_interpolation text do |scan|
          escapes = scan[2].size
          res << scan.matched[0...-2 - escapes]
          if escapes % 2 == 1
            res << "\\" * (escapes - 1) << '#{'
          else
            res << "\\" * [0, escapes - 1].max
            res << Script::Parser.new(scan, line, scan.pos - scan.matched_size, filename).
              parse_interpolated.perform(environment).to_s
          end
        end
        res + rest
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
