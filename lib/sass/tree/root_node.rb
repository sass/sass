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
        result, extends = Visitors::Cssize.visit(result)
        result.do_extend(extends).to_s
      end

      def do_extend(extends)
        return self if extends.empty?
        result = super
        extends.each do |e, by|
          unless e.extended?
            Sass::Util.sass_warn <<WARN
WARNING on line #{by.line}#{" of #{by.filename}" if by.filename}:
  Missing selector #{e.inspect} not found for @extend of #{by.inspect}.
  This will become an error in a future release.
  To allow this condition, change to:
    @extend #{e.inspect} !optional
WARN
          end
        end
        result
      end
    end
  end
end
