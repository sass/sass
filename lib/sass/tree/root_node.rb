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

      # Runs the dynamic Sass code *and* computes the CSS for the tree.
      # @see #to_s
      def render
        result, extends = Visitors::Cssize.visit(Visitors::Perform.visit(self))
        result = result.do_extend(extends) unless extends.empty?
        result.to_s
      end

      # Returns an error message if the given child node is invalid,
      # and false otherwise.
      #
      # Only property nodes are invalid at root level.
      #
      # @see Node#invalid_child?
      def invalid_child?(child)
        case child
        when Tree::ExtendNode
          "Extend directives may only be used within rules."
        when Tree::PropNode
          "Properties aren't allowed at the root of a document." +
            child.pseudo_class_selector_message
        else
          return
        end
      end
    end
  end
end
