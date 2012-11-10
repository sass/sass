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

      # The array of {SimpleSequence simple selector sequences}, operators, and
      # newlines. The operators are strings such as `"+"` and `">"` representing
      # the corresponding CSS operators, or interpolated SassScript. Newlines
      # are also newline strings; these aren't semantically relevant, but they
      # do affect formatting.
      #
      # @return [Array<SimpleSequence, String|Array<Sass::Tree::Node, String>>]
      attr_reader :members

      # @param seqs_and_ops [Array<SimpleSequence, String|Array<Sass::Tree::Node, String>>] See \{#members}
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
        members = @members.dup
        nl = (members.first == "\n" && members.shift)
        unless members.any? do |seq_or_op|
            seq_or_op.is_a?(SimpleSequence) && seq_or_op.members.first.is_a?(Parent)
          end
          old_members, members = members, []
          members << nl if nl
          members << SimpleSequence.new([Parent.new], false)
          members += old_members
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
      # @overload def do_extend(extends, parent_directives)
      # @param extends [Sass::Util::SubsetMap{Selector::Simple =>
      #                                       Sass::Tree::Visitors::Cssize::Extend}]
      #   The extensions to perform on this selector
      # @param parent_directives [Array<Sass::Tree::DirectiveNode>]
      #   The directives containing this selector.
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      #   These correspond to a {CommaSequence}'s {CommaSequence#members members array}.
      # @see CommaSequence#do_extend
      def do_extend(extends, parent_directives, seen = Set.new)
        extended_not_expanded = members.map do |sseq_or_op|
          next [[sseq_or_op]] unless sseq_or_op.is_a?(SimpleSequence)
          extended = sseq_or_op.do_extend(extends, parent_directives, seen)
          choices = extended.map {|seq| seq.members}
          choices.unshift([sseq_or_op]) unless extended.any? {|seq| seq.superselector?(sseq_or_op)}
          choices
        end
        weaves = Sass::Util.paths(extended_not_expanded).map {|path| weave(path)}
        Sass::Util.flatten(trim(weaves), 1).map {|p| Sequence.new(p)}
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

      # Add to the {SimpleSequence#sources} sets of the child simple sequences.
      # This destructively modifies this sequence's members array, but not the
      # child simple sequences.
      #
      # @param sources [Set<Sequence>]
      def add_sources!(sources)
        members.map! {|m| m.is_a?(SimpleSequence) ? m.with_more_sources(sources) : m}
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
        # This function works by moving through the selector path left-to-right,
        # building all possible prefixes simultaneously. These prefixes are
        # `befores`, while the remaining parenthesized suffixes is `afters`.
        befores = [[]]
        afters = path.dup

        until afters.empty?
          current = afters.shift.dup
          last_current = [current.pop]
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
      # Semantically, for selectors A and B, this returns all selectors `AB_i`
      # such that the union over all i of elements matched by `AB_i X` is
      # identical to the intersection of all elements matched by `A X` and all
      # elements matched by `B X`. Some `AB_i` are elided to reduce the size of
      # the output.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Array<Array<SimpleSequence or String>>]
      def subweave(seq1, seq2)
        return [seq2] if seq1.empty?
        return [seq1] if seq2.empty?

        seq1, seq2 = seq1.dup, seq2.dup
        return unless init = merge_initial_ops(seq1, seq2)
        return unless fin = merge_final_ops(seq1, seq2)
        seq1 = group_selectors(seq1)
        seq2 = group_selectors(seq2)
        lcs = Sass::Util.lcs(seq2, seq1) do |s1, s2|
          next s1 if s1 == s2
          next unless s1.first.is_a?(SimpleSequence) && s2.first.is_a?(SimpleSequence)
          next s2 if parent_superselector?(s1, s2)
          next s1 if parent_superselector?(s2, s1)
        end

        diff = [[init]]
        until lcs.empty?
          diff << chunks(seq1, seq2) {|s| parent_superselector?(s.first, lcs.first)} << [lcs.shift]
          seq1.shift
          seq2.shift
        end
        diff << chunks(seq1, seq2) {|s| s.empty?}
        diff += fin.map {|sel| sel.is_a?(Array) ? sel : [sel]}
        diff.reject! {|c| c.empty?}

        Sass::Util.paths(diff).map {|p| p.flatten}.reject {|p| path_has_two_subjects?(p)}
      end

      # Extracts initial selector combinators (`"+"`, `">"`, `"~"`, and `"\n"`)
      # from two sequences and merges them together into a single array of
      # selector combinators.
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

      # Extracts final selector combinators (`"+"`, `">"`, `"~"`) and the
      # selectors to which they apply from two sequences and merges them
      # together into a single array.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Array<SimpleSequence or String or
      #     Array<Array<SimpleSequence or String>>]
      #   If there are no trailing combinators to be merged, this will be the
      #   empty array. If the trailing combinators cannot be merged, this will
      #   be nil. Otherwise, this will contained the merged selector. Array
      #   elements are [Sass::Util#paths]-style options; conceptually, an "or"
      #   of multiple selectors.
      def merge_final_ops(seq1, seq2, res = [])
        ops1, ops2 = [], []
        ops1 << seq1.pop while seq1.last.is_a?(String)
        ops2 << seq2.pop while seq2.last.is_a?(String)

        # Not worth the headache of trying to preserve newlines here. The most
        # important use of newlines is at the beginning of the selector to wrap
        # across lines anyway.
        ops1.reject! {|o| o == "\n"}
        ops2.reject! {|o| o == "\n"}

        return res if ops1.empty? && ops2.empty?
        if ops1.size > 1 || ops2.size > 1
          # If there are multiple operators, something hacky's going on. If one
          # is a supersequence of the other, use that, otherwise give up.
          lcs = Sass::Util.lcs(ops1, ops2)
          return unless lcs == ops1 || lcs == ops2
          res.unshift *(ops1.size > ops2.size ? ops1 : ops2).reverse
          return res
        end

        # This code looks complicated, but it's actually just a bunch of special
        # cases for interactions between different combinators.
        op1, op2 = ops1.first, ops2.first
        if op1 && op2
          sel1 = seq1.pop
          sel2 = seq2.pop
          if op1 == '~' && op2 == '~'
            if sel1.superselector?(sel2)
              res.unshift sel2, '~'
            elsif sel2.superselector?(sel1)
              res.unshift sel1, '~'
            else
              merged = sel1.unify(sel2.members, sel2.subject?)
              res.unshift [
                [sel1, '~', sel2, '~'],
                [sel2, '~', sel1, '~'],
                ([merged, '~'] if merged)
              ].compact
            end
          elsif (op1 == '~' && op2 == '+') || (op1 == '+' && op2 == '~')
            if op1 == '~'
              tilde_sel, plus_sel = sel1, sel2
            else
              tilde_sel, plus_sel = sel2, sel1
            end

            if tilde_sel.superselector?(plus_sel)
              res.unshift plus_sel, '+'
            else
              merged = plus_sel.unify(tilde_sel.members, tilde_sel.subject?)
              res.unshift [
                [tilde_sel, '~', plus_sel, '+'],
                ([merged, '+'] if merged)
              ].compact
            end
          elsif op1 == '>' && %w[~ +].include?(op2)
            res.unshift sel2, op2
            seq1.push sel1, op1
          elsif op2 == '>' && %w[~ +].include?(op1)
            res.unshift sel1, op1
            seq2.push sel2, op2
          elsif op1 == op2
            return unless merged = sel1.unify(sel2.members, sel2.subject?)
            res.unshift merged, op1
          else
            # Unknown selector combinators can't be unified
            return
          end
          return merge_final_ops(seq1, seq2, res)
        elsif op1
          seq2.pop if op1 == '>' && seq2.last && seq2.last.superselector?(seq1.last)
          res.unshift seq1.pop, op1
          return merge_final_ops(seq1, seq2, res)
        else # op2
          seq1.pop if op2 == '>' && seq1.last && seq1.last.superselector?(seq2.last)
          res.unshift seq2.pop, op2
          return merge_final_ops(seq1, seq2, res)
        end
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
      # @return [Array<Array>] All possible orderings of the initial subsequences.
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

      # Given two selector sequences, returns whether `seq1` is a
      # superselector of `seq2`; that is, whether `seq1` matches every
      # element `seq2` matches.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Boolean]
      def _superselector?(seq1, seq2)
        seq1 = seq1.reject {|e| e == "\n"}
        seq2 = seq2.reject {|e| e == "\n"}
        # Selectors with leading or trailing operators are neither
        # superselectors nor subselectors.
        return if seq1.last.is_a?(String) || seq2.last.is_a?(String) ||
          seq1.first.is_a?(String) || seq2.first.is_a?(String)
        # More complex selectors are never superselectors of less complex ones
        return if seq1.size > seq2.size
        return seq1.first.superselector?(seq2.last) if seq1.size == 1

        _, si = Sass::Util.enum_with_index(seq2).find do |e, i|
          return if i == seq2.size - 1
          next if e.is_a?(String)
          seq1.first.superselector?(e)
        end
        return unless si

        if seq1[1].is_a?(String)
          return unless seq2[si+1].is_a?(String)
          # .foo ~ .bar is a superselector of .foo + .bar
          return unless seq1[1] == "~" ? seq2[si+1] != ">" : seq1[1] == seq2[si+1]
          return _superselector?(seq1[2..-1], seq2[si+2..-1])
        elsif seq2[si+1].is_a?(String)
          return unless seq2[si+1] == ">"
          return _superselector?(seq1[1..-1], seq2[si+2..-1])
        else
          return _superselector?(seq1[1..-1], seq2[si+1..-1])
        end
      end

      # Like \{#_superselector?}, but compares the selectors in the
      # context of parent selectors, as though they shared an implicit
      # base simple selector. For example, `B` is not normally a
      # superselector of `B A`, since it doesn't match `A` elements.
      # However, it is a parent superselector, since `B X` is a
      # superselector of `B A X`.
      #
      # @param seq1 [Array<SimpleSequence or String>]
      # @param seq2 [Array<SimpleSequence or String>]
      # @return [Boolean]
      def parent_superselector?(seq1, seq2)
        base = Sass::Selector::SimpleSequence.new([Sass::Selector::Placeholder.new('<temp>')], false)
        _superselector?(seq1 + [base], seq2 + [base])
      end

      # Removes redundant selectors from between multiple lists of
      # selectors. This takes a list of lists of selector sequences;
      # each individual list is assumed to have no redundancy within
      # itself. A selector is only removed if it's redundant with a
      # selector in another list.
      #
      # "Redundant" here means that one selector is a superselector of
      # the other. The more specific selector is removed.
      #
      # @param seqses [Array<Array<Array<SimpleSequence or String>>>]
      # @return [Array<Array<Array<SimpleSequence or String>>>]
      def trim(seqses)
        # Avoid truly horrific quadratic behavior. TOOD: I think there
        # may be a way to get perfect trimming without going quadratic.
        return seqses if seqses.size > 100
        # This is n^2 on the sequences, but only comparing between
        # separate sequences should limit the quadratic behavior.
        seqses.map do |seqs1|
          seqs1.reject do |seq1|
            min_spec = 0
            _sources(seq1).map {|seq| min_spec += seq.specificity}
            seqses.any? do |seqs2|
              next if seqs1.equal?(seqs2)
              # Second Law of Extend: the specificity of a generated selector
              # should never be less than the specificity of the extending
              # selector.
              #
              # See https://github.com/nex3/sass/issues/324.
              seqs2.any? {|seq2| _specificity(seq2) >= min_spec && _superselector?(seq2, seq1)}
            end
          end
        end
      end

      def _hash
        members.reject {|m| m == "\n"}.hash
      end

      def _eql?(other)
        other.members.reject {|m| m == "\n"}.eql?(self.members.reject {|m| m == "\n"})
      end

      private

      def path_has_two_subjects?(path)
        subject = false
        path.each do |sseq_or_op|
          next unless sseq_or_op.is_a?(SimpleSequence)
          next unless sseq_or_op.subject?
          return true if subject
          subject = true
        end
        false
      end

      def _sources(seq)
        s = Set.new
        seq.map {|sseq_or_op| s.merge sseq_or_op.sources if sseq_or_op.is_a?(SimpleSequence)}
        s
      end

      def extended_not_expanded_to_s(extended_not_expanded)
        extended_not_expanded.map do |choices|
          choices = choices.map do |sel|
            next sel.first.to_s if sel.size == 1
            "#{sel.join ' '}"
          end
          next choices.first if choices.size == 1 && !choices.include?(' ')
          "(#{choices.join ', '})"
        end.join ' '
      end
    end
  end
end
