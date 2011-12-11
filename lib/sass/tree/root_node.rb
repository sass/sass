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
        Visitors::CheckNesting.visit(self)
        result = Visitors::Perform.visit(self)
        Visitors::CheckNesting.visit(result) # Check again to validate mixins
        result, extends, silenced = Visitors::Cssize.visit(result)
        result = result.do_extend(extends, silenced) unless extends.empty? && silenced.empty?
        result.to_s
      end
    end
  end
end
