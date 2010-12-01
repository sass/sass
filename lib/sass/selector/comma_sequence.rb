module Sass
  module Selector
    # A comma-separated sequence of selectors.
    class CommaSequence < AbstractSequence
      # The comma-separated selector sequences
      # represented by this class.
      #
      # @return [Array<Sequence>]
      attr_reader :members

      # @param seqs [Array<Sequence>] See \{#members}
      def initialize(seqs)
        @members = seqs
      end

      # Resolves the {Parent} selectors within this selector
      # by replacing them with the given parent selector,
      # handling commas appropriately.
      #
      # @param super_cseq [CommaSequence] The parent selector
      # @return [CommaSequence] This selector, with parent references resolved
      # @raise [Sass::SyntaxError] If a parent selector is invalid
      def resolve_parent_refs(super_cseq)
        if super_cseq.nil?
          if @members.any? do |sel|
              sel.members.any? do |sel_or_op|
                sel_or_op.is_a?(SimpleSequence) && sel_or_op.members.any? {|ssel| ssel.is_a?(Parent)}
              end
            end
            raise Sass::SyntaxError.new("Base-level rules cannot contain the parent-selector-referencing character '&'.")
          end
          return self
        end

        CommaSequence.new(
          super_cseq.members.map do |super_seq|
            @members.map {|seq| seq.resolve_parent_refs(super_seq)}
          end.flatten)
      end

      # Non-destrucively extends this selector with the extensions specified in a hash
      # (which should come from {Sass::Tree::Visitors::Cssize}).
      #
      # @todo Link this to the reference documentation on `@extend`
      #   when such a thing exists.
      #
      # @param extends [Sass::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions to perform on this selector
      # @return [CommaSequence] A copy of this selector,
      #   with extensions made according to `extends`
      def do_extend(extends)
        CommaSequence.new(members.map {|seq| seq.do_extend(extends)}.flatten)
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join(", ")
      end

      # @see Simple#to_a
      def to_a
        arr = Sass::Util.intersperse(@members.map {|m| m.to_a}, ", ").flatten
        arr.delete("\n")
        arr
      end

      private

      def _hash
        members.hash
      end

      def _eql?(other)
        other.class == self.class && other.members.eql?(self.members)
      end
    end
  end
end
