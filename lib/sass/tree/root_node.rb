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

      # @see Node#to_s
      def to_s(*args)
        super
      rescue Sass::SyntaxError => e
        e.sass_template = @template
        raise e
      end

      # @see Node#perform
      def perform(environment)
        environment.options = @options if environment.options.nil? || environment.options.empty?
        super
      rescue Sass::SyntaxError => e
        e.sass_template = @template
        raise e
      end

      # @see Node#cssize
      def cssize(*args)
        super
      rescue Sass::SyntaxError => e
        e.sass_template = @template
        raise e
      end

      # @see \{Node#perform!}
      def perform!(environment)
        environment.options = @options if environment.options.nil? || environment.options.empty?
        super
      end

      protected

      # Destructively converts this static Sass node into a static CSS node,
      # and checks that there are no properties at root level.
      #
      # @param parent [Node, nil] The parent node of this node.
      #   This should only be non-nil if the parent is the same class as this node
      # @see Node#cssize!
      def cssize!(parent)
        super
        return unless child = children.find {|c| c.is_a?(PropNode)}
        message = "Properties aren't allowed at the root of a document." +
          child.pseudo_class_selector_message
        raise Sass::SyntaxError.new(message, :line => child.line)
      end

      # Computes the CSS corresponding to this Sass tree.
      #
      # @param args [Array] ignored
      # @return [String] The resulting CSS
      # @see Sass::Tree
      def _to_s(*args)
        result = String.new
        children.each do |child|
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
