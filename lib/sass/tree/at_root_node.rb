module Sass
  module Tree
    # A dynamic node representing an `@at-root` directive.
    #
    # An `@at-root` directive with a selector is converted to an \{AtRootNode}
    # containing a \{RuleNode} at parse time.
    #
    # @see Sass::Tree
    class AtRootNode < Node
      def bubbles?
        true
      end
    end
  end
end
