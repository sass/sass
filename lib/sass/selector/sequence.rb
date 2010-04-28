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
        members.slice!(0) if nl = (members.first == "\n")
        unless members.any? do |seq_or_op|
            seq_or_op.is_a?(SimpleSequence) && seq_or_op.members.first.is_a?(Parent)
          end
          members = []
          members << "\n" if nl
          members << SimpleSequence.new([Parent.new])
          members += @members
        end

        Sequence.new(
          members.map do |seq_or_op|
            next seq_or_op unless seq_or_op.is_a?(SimpleSequence)
            seq_or_op.resolve_parent_refs(super_seq)
          end.flatten)
      end

      # Non-destructively extends this selector
      # with the extensions specified in a hash
      # (which should be populated via {Sass::Tree::Node#cssize}).
      #
      # @overload def do_extend(extends)
      # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions to perform on this selector
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      #   These correspond to a {CommaSequence}'s {CommaSequence#members members array}.
      # @see CommaSequence#do_extend
      def do_extend(extends, supers = [])
        paths = Haml::Util.paths(members.map do |sseq_or_op|
            next [[sseq_or_op]] unless sseq_or_op.is_a?(SimpleSequence)
            extended = sseq_or_op.do_extend(extends, supers)
            choices = extended.map {|seq| seq.members}
            choices.unshift([sseq_or_op]) unless extended.any? {|seq| seq.superselector?(sseq_or_op)}
            choices
          end)
        Haml::Util.flatten(paths.map {|path| weave(path)}, 1).map {|p| Sequence.new(p)}
      end

      # Returns whether or not this selector matches all elements
      # that the given selector matches (as well as possibly more).
      #
      # @example
      # (.foo).superselector?(.foo.bar) #=> true
      # (.foo).superselector?(.bar) #=> false
      # (.bar .foo).superselector?(.foo) #=> false
      #
      # @param sseq [SimpleSequence]
      # @return [Boolean]
      def superselector?(sseq)
        return false unless members.size == 1
        members.last.superselector?(sseq)
      end

      # @see Simple#to_a
      def to_a
        ary = @members.map {|seq_or_op| seq_or_op.is_a?(SimpleSequence) ? seq_or_op.to_a : seq_or_op}
        ary = Haml::Util.intersperse(ary, " ")
        ary = Haml::Util.substitute(ary, [" ", "\n", " "], ["\n"])
        ary.flatten.compact
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join(" ")
      end

      # Returns a hash code for this sequence.
      #
      # @return [Fixnum]
      def hash
        members.reject {|m| m == "\n"}.hash
      end

      # Checks equality between this and another object.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class &&
          other.members.reject {|m| m == "\n"}.eql?(self.members.reject {|m| m == "\n"})
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
          befores = Haml::Util.flatten(befores.map do |before|
              subweave(before, current).map {|seqs| seqs + last_current}
            end, 1)
          return befores if afters.empty?
        end
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
      # @overload def subweave(seq1, seq2)
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Array<Array<SimpleSequence or String>>]
      def subweave(seq1, seq2, cache = {})
        return [seq2] if seq1.empty?
        return [seq1] if seq2.empty?
        cache[[seq1, seq2]] ||=
          begin
            sseq1, rest1 = seq_split(seq1)
            sseq2, rest2 = seq_split(seq2)

            if sseq1.eql?(sseq2)
              subweave(rest1, rest2, cache).map {|subseq| sseq1 + subseq}
            else
              unified = unify_heads(sseq1, sseq2) || unify_heads(sseq2, sseq1)
              res = []
              subweave(rest1, seq2, cache).each {|subseq| res << sseq1 + subseq}
              subweave(rest1, rest2, cache).each {|subseq| res << unified + subseq} if unified
              subweave(seq1, rest2, cache).each {|subseq| res << sseq2 + subseq}
              res
            end
          end
      end

      def seq_split(seq)
        tail = seq.dup
        head = []
        begin
          head << tail.shift
        end while !tail.empty? && head.last.is_a?(String) || tail.first.is_a?(String)
        return head, tail
      end

      def unify_heads(sseq1, sseq2)
        return unless sseq2.size == 1 # Can't unify ".foo > .bar" and ".baz > .bang"
        unified = sseq1.last.unify(sseq2.last.members) unless sseq1.last.is_a?(String) || sseq2.last.is_a?(String)
        sseq1[0...-1] << unified if unified
      end
    end
  end
end
