module Sass
  module Tree
    # A static node that is the root node of the Sass document.
    class RootNode < Node
      # The Sass template from which this node was created
      #
      # @param template [String]
      attr_reader :template

      # @param template [String] The Sass template from which this node was created
      def initialize(template)
        super()
        @template = template
      end

      # @see \{Node#to\_s}
      def to_s(*args)
        super
      rescue Sass::SyntaxError => e
        e.sass_template = @template
        raise e
      end

      # @see \{Node#perform}
      def perform(*args)
        super
      rescue Sass::SyntaxError => e
        e.sass_template = @template
        raise e
      end

      protected

      # Computes the CSS corresponding to this Sass tree.
      #
      # @param args [Array] ignored
      # @return [String] The resulting CSS
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see Sass::Tree
      def _to_s(*args)
        result = String.new
        children.each do |child|
          if child.is_a? PropNode
            message = "Properties aren't allowed at the root of a document." +
              child.pseudo_class_selector_message
            raise Sass::SyntaxError.new(message, :line => child.line)
          end

          next if child.invisible?
          child_str = child.to_s(1)
          result << child_str + (style == :compressed ? '' : "\n")
        end
        result.rstrip!
        return "" if result.empty?
        return result + "\n"
      end
    end
  end
end
