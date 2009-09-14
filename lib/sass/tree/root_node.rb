module Sass
  module Tree
    # A static node that is the root node of the Sass document.
    class RootNode < Node
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
          raise Sass::SyntaxError.new('Properties aren\'t allowed at the root of a document.',
            :line => child.line) if child.is_a? PropNode

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
