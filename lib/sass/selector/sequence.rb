module Sass
  module Selector
    # An operator-separated sequence of
    # {SimpleSequence simple selector sequences}.
    class Sequence < AbstractSequence
      # Sets the line of the Sass template on which this selector was declared.
      # This also sets the line for all child selectors.
      #
      # @param line [Fixnum]
      # @return [Fixnum]
      def line=(line)
        members.each {|m| m.line = line if m.is_a?(SimpleSequence)}
        line
      end

      # Sets the name of the file in which this selector was declared,
      # or `nil` if it was not declared in a file (e.g. on stdin).
      # This also sets the filename for all child selectors.
      #
      # @param filename [String, nil]
      # @return [String, nil]
      def filename=(filename)
        members.each {|m| m.filename = filename if m.is_a?(SimpleSequence)}
        filename
      end

      # The array of {SimpleSequence simple selector sequences}, operators, and newlines.
      # The operators are strings such as `"+"` and `">"`
      # representing the corresponding CSS operators.
      # Newlines are also newline strings;
      # these aren't semantically relevant,
      # but they do affect formatting.
      #
      # @return [Array<SimpleSequence, String>]
      attr_reader :members

      # @param seqs_and_ops [Array<SimpleSequence, String>] See \{#members}
      def initialize(seqs_and_ops)
        @members = seqs_and_ops
      end

      # Resolves the {Parent} selectors within this selector
      # by replacing them with the given parent selector,
      # handling commas appropriately.
      #
      # @param super_seq [Sequence] The parent selector sequence
      # @return [Sequence] This selector, with parent references resolved
      # @raise [Sass::SyntaxError] If a parent selector is invalid
      def resolve_parent_refs(super_seq)
        members = @members
        nl = (members.first == "\n" && members.shift)
        unless members.any? do |seq_or_op|
            seq_or_op.is_a?(SimpleSequence) && seq_or_op.members.first.is_a?(Parent)
          end
          members = []
          members << nl if nl
          members << SimpleSequence.new([Parent.new])
          members += @members
        end

        Sequence.new(
          members.map do |seq_or_op|
            next seq_or_op unless seq_or_op.is_a?(SimpleSequence)
            seq_or_op.resolve_parent_refs(super_seq)
          end.flatten)
      end

      # Non-destructively extends this selector with the extensions specified in a hash
      # (which should come from {Sass::Tree::Visitors::Cssize}).
      #
      # @overload def do_extend(extends)
      # @param extends [Sass::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions to perform on this selector
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      #   These correspond to a {CommaSequence}'s {CommaSequence#members members array}.
      # @see CommaSequence#do_extend
      def do_extend(extends, seen = Set.new)
        paths = Sass::Util.paths(members.map do |sseq_or_op|
            next [[sseq_or_op]] unless sseq_or_op.is_a?(SimpleSequence)
            extended = sseq_or_op.do_extend(extends, seen)
            choices = extended.map {|seq| seq.members}
            choices.unshift([sseq_or_op]) unless extended.any? {|seq| seq.superselector?(sseq_or_op)}
            choices
          end)
        Sass::Util.flatten(paths.map {|path| weave(path)}, 1).map {|p| Sequence.new(p)}
      end

      # Returns whether or not this selector matches all elements
      # that the given selector matches (as well as possibly more).
      #
      # @example
      #   (.foo).superselector?(.foo.bar) #=> true
      #   (.foo).superselector?(.bar) #=> false
      #   (.bar .foo).superselector?(.foo) #=> false
      # @param sseq [SimpleSequence]
      # @return [Boolean]
      def superselector?(sseq)
        return false unless members.size == 1
        members.last.superselector?(sseq)
      end

      # @see Simple#to_a
      def to_a
        ary = @members.map {|seq_or_op| seq_or_op.is_a?(SimpleSequence) ? seq_or_op.to_a : seq_or_op}
        Sass::Util.intersperse(ary, " ").flatten.compact
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join(" ")
      end

      private

      # Conceptually, this expands "parenthesized selectors".
      # That is, if we have `.A .B {@extend .C}` and `.D .C {...}`,
      # this conceptually expands into `.D .C, .D (.A .B)`,
      # and this function translates `.D (.A .B)` into `.D .A .B, .A.D .B, .D .A .B`.
      #
      # @param path [Array<Array<SimpleSequence or String>>] A list of parenthesized selector groups.
      # @return [Array<Array<SimpleSequence or String>>] A list of fully-expanded selectors.
      def weave(path)
        befores = [[]]
        afters = path.dup

        until afters.empty?
          current = afters.shift.dup
          last_current = [current.pop]
          while !current.empty? && last_current.first.is_a?(String) || current.last.is_a?(String)
            last_current.unshift(current.pop)
          end
          befores = Sass::Util.flatten(befores.map do |before|
              next [] unless sub = subweave(before, current)
              sub.map {|seqs| seqs + last_current}
            end, 1)
        end
        return befores
      end

      # This interweaves two lists of selectors,
      # returning all possible orderings of them (including using unification)
      # that maintain the relative ordering of the input arrays.
      #
      # For example, given `.foo .bar` and `.baz .bang`,
      # this would return `.foo .bar .baz .bang`, `.foo .bar.baz .bang`,
      # `.foo .baz .bar .bang`, `.foo .baz .bar.bang`, `.foo .baz .bang .bar`,
      # and so on until `.baz .bang .foo .bar`.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Array<Array<SimpleSequence or String>>]
      def subweave(seq1, seq2)
        return [seq2] if seq1.empty?
        return [seq1] if seq2.empty?

        return unless init = merge_initial_ops(seq1, seq2)
        seq1 = group_selectors(seq1)
        seq2 = group_selectors(seq2)
        lcs = Sass::Util.lcs(seq2, seq1) do |s1, s2|
          next s1 if s1 == s2
          next unless s1.first.is_a?(SimpleSequence) && s2.first.is_a?(SimpleSequence)
          next s2 if subweave_superselector?(s1, s2)
          next s1 if subweave_superselector?(s2, s1)
        end

        diff = [[init]]
        until lcs.empty?
          diff << chunks(seq1, seq2) {|s| subweave_superselector?(s.first, lcs.first)} << [lcs.shift]
          seq1.shift
          seq2.shift
        end
        diff << chunks(seq1, seq2) {|s| s.empty?}
        diff.reject! {|c| c.empty?}

        Sass::Util.paths(diff).map {|p| p.flatten}
      end

      # Extracts initial selector operators (`"+"`, `">"`, `"~"`, and `"\n"`)
      # from two sequences and merges them together into a single array of
      # selector operators.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Array<String>, nil] If there are no operators in the merged
      #   sequence, this will be the empty array. If the operators cannot be
      #   merged, this will be nil.
      def merge_initial_ops(seq1, seq2)
        ops1, ops2 = [], []
        ops1 << seq1.shift while seq1.first.is_a?(String)
        ops2 << seq2.shift while seq2.first.is_a?(String)

        newline = false
        newline ||= !!ops1.shift if ops1.first == "\n"
        newline ||= !!ops2.shift if ops2.first == "\n"

        # If neither sequence is a subsequence of the other, they cannot be
        # merged successfully
        lcs = Sass::Util.lcs(ops1, ops2)
        return unless lcs == ops1 || lcs == ops2
        return (newline ? ["\n"] : []) + (ops1.size > ops2.size ? ops1 : ops2)
      end

      # Takes initial subsequences of `seq1` and `seq2` and returns all
      # orderings of those subsequences. The initial subsequences are determined
      # by a block.
      #
      # Destructively removes the initial subsequences of `seq1` and `seq2`.
      #
      # For example, given `(A B C | D E)` and `(1 2 | 3 4 5)` (with `|`
      # denoting the boundary of the initial subsequence), this would return
      # `[(A B C 1 2), (1 2 A B C)]`. The sequences would then be `(D E)` and
      # `(3 4 5)`.
      #
      # @param seq1 [Array]
      # @param seq2 [Array]
      # @yield [a] Used to determine when to cut off the initial subsequences.
      #   Called repeatedly for each sequence until it returns true.
      # @yieldparam a [Array] A final subsequence of one input sequence after
      #   cutting off some initial subsequence.
      # @yieldreturn [Boolean] Whether or not to cut off the initial subsequence
      #   here.
      def chunks(seq1, seq2)
        chunk1 = []
        chunk1 << seq1.shift until yield seq1
        chunk2 = []
        chunk2 << seq2.shift until yield seq2
        return [] if chunk1.empty? && chunk2.empty?
        return [chunk2] if chunk1.empty?
        return [chunk1] if chunk2.empty?
        [chunk1 + chunk2, chunk2 + chunk1]
      end

      # Groups a sequence into subsequences. The subsequences are determined by
      # strings; adjacent non-string elements will be put into separate groups,
      # but any element adjacent to a string will be grouped with that string.
      #
      # For example, `(A B "C" D E "F" G "H" "I" J)` will become `[(A) (B "C" D)
      # (E "F" G "H" "I" J)]`.
      #
      # @param seq [Array]
      # @return [Array<Array>]
      def group_selectors(seq)
        newseq = []
        tail = seq.dup
        until tail.empty?
          head = []
          begin
            head << tail.shift
          end while !tail.empty? && head.last.is_a?(String) || tail.first.is_a?(String)
          newseq << head
        end
        return newseq
      end

      # Given two sequences of simple selectors, returns whether `sseq1` is a
      # superselector of `sseq2`.
      #
      # @param sseq1 [Array<SimpleSelector or String>]
      # @param sseq2 [Array<SimpleSelector or String>]
      # @return [Boolean]
      def subweave_superselector?(sseq1, sseq2)
        if sseq1.size > 1
          # More complex selectors are never superselectors of less complex ones
          return unless sseq2.size > 1
          # .foo ~ .bar is a superselector of .foo + .bar
          return unless sseq1[1] == "~" ? sseq2[1] != ">" : sseq2[1] == sseq1[1]
          return unless sseq1.first.superselector?(sseq2.first)
          return true if sseq1.size == 2
          return false if sseq2.size == 2
          return subweave_superselector?(sseq1[2..-1], sseq2[2..-1])
        elsif sseq2.size > 1
          return true if sseq2[1] == ">" && sseq1.first.superselector?(sseq2.first)
          return false if sseq2.size == 2
          return subweave_superselector?(sseq1, sseq2[2..-1])
        else
          sseq1.first.superselector?(sseq2.first)
        end
      end

      def _hash
        members.reject {|m| m == "\n"}.hash
      end

      def _eql?(other)
        other.members.reject {|m| m == "\n"}.eql?(self.members.reject {|m| m == "\n"})
      end
    end
  end
end
