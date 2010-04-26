module Sass
  module Selector
    # A unseparated sequence of selectors
    # that all apply to a single element.
    # For example, `.foo#bar[attr=baz]` is a simple sequence
    # of the selectors `.foo`, `#bar`, and `[attr=baz]`.
    class SimpleSequence < AbstractSequence
      # The array of individual selectors.
      #
      # @return [Array<Simple>]
      attr_reader :members

      # Returns the element or universal selector in this sequence,
      # if it exists.
      #
      # @return [Element, Universal, nil]
      def base
        @base ||= (members.first if members.first.is_a?(Element) || members.first.is_a?(Universal))
      end

      # Returns the non-base selectors in this sequence.
      #
      # @return [Set<Simple>]
      def rest
        @rest ||= Set.new(base ? members[1..-1] : members)
      end

      # @param selectors [Array<Simple>] See \{#members}
      def initialize(selectors)
        @members = selectors
      end

      # Resolves the {Parent} selectors within this selector
      # by replacing them with the given parent selector,
      # handling commas appropriately.
      #
      # @param super_seq [Sequence] The parent selector sequence
      # @return [Array<SimpleSequence>] This selector, with parent references resolved.
      #   This is an array because the parent selector is itself a {Sequence}
      # @raise [Sass::SyntaxError] If a parent selector is invalid
      def resolve_parent_refs(super_seq)
        # Parent selector only appears as the first selector in the sequence
        return [self] unless @members.first.is_a?(Parent)

        return super_seq.members if @members.size == 1
        unless super_seq.members.last.is_a?(SimpleSequence)
          raise Sass::SyntaxError.new("Invalid parent selector: " + super_seq.to_a.join)
        end

        super_seq.members[0...-1] +
          [SimpleSequence.new(super_seq.members.last.members + @members[1..-1])]
      end

      # Non-destrucively extends this selector
      # with the extensions specified in a hash
      # (which should be populated via {Sass::Tree::Node#cssize}).
      #
      # @overload def do_extend(extends)
      # @param extends [{Selector::Simple => Selector::Sequence}]
      #   The extensions to perform on this selector
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      # @see CommaSequence#do_extend
      def do_extend(extends, supers = [])
        seqs = extends.get(members.to_set).map do |seq, sels|
          # If A {@extend B} and C {...},
          # seq is A, sels is B, and self is C

          self_without_sel = self.members - sels
          next unless unified = seq.members.last.unify(self_without_sel)
          [sels, seq.members[0...-1] + [unified]]
        end.compact.map {|sels, seq| [sels, Sequence.new(seq)]}

        seqs.map {|_, seq| seq}.concat(
          seqs.map do |sels, seq|
            new_seqs = seq.do_extend(extends, supers.unshift(sels))[1..-1]
            supers.shift
            new_seqs
          end.flatten.uniq)
      rescue SystemStackError
        handle_extend_loop(supers)
      end

      # Unifies this selector with another {SimpleSequence}'s {SimpleSequence#members members array},
      # returning another `SimpleSequence`
      # that matches both this selector and the input selector.
      #
      # @param sels [Array<Simple>] A {SimpleSequence}'s {SimpleSequence#members members array}
      # @return [SimpleSequence, nil] A {SimpleSequence} matching both `sels` and this selector,
      #   or `nil` if this is impossible (e.g. unifying `#foo` and `#bar`)
      # @raise [Sass::SyntaxError] If this selector cannot be unified.
      #   This will only ever occur when a dynamic selector,
      #   such as {Parent} or {Interpolation}, is used in unification.
      #   Since these selectors should be resolved
      #   by the time extension and unification happen,
      #   this exception will only ever be raised as a result of programmer error
      def unify(sels)
        return unless sseq = members.inject(sels) do |sseq, sel|
          return unless sseq
          sel.unify(sseq)
        end
        SimpleSequence.new(sseq)
      end

      # @see Simple#to_a
      def to_a
        @members.map {|sel| sel.to_a}.flatten
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join
      end

      # Returns a hash code for this sequence.
      #
      # @return [Fixnum]
      def hash
        [base, rest].hash
      end

      # Checks equality between this and another object.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class && other.base.eql?(self.base) && other.rest.eql?(self.rest)
      end

      private

      # Raise a {Sass::SyntaxError} describing a loop of `@extend` directives.
      #
      # @param supers [Array<Simple>] The stack of selectors that contains the loop,
      #   ordered from deepest to most shallow.
      # @raise [Sass::SyntaxError] Describing the loop
      def handle_extend_loop(supers)
        supers.inject([]) do |sseqs, sseq|
          next sseqs.push(sseq) unless sseqs.first.eql?(sseq)
          conses = Haml::Util.enum_cons(sseqs.push(sseq), 2).to_a
          _, i = Haml::Util.enum_with_index(conses).max do |((_, sseq1), _), ((_, sseq2), _)|
            sseq1.first.line <=> sseq2.first.line
          end
          loop = (conses[i..-1] + conses[0...i]).map do |sseq1, sseq2|
            sel1 = SimpleSequence.new(sseq1).inspect
            sel2 = SimpleSequence.new(sseq2).inspect
            str = "  #{sel1} extends #{sel2} on line #{sseq2.first.line}"
            str << " of " << sseq2.first.filename if sseq2.first.filename
            str
          end.join(",\n")
          raise Sass::SyntaxError.new("An @extend loop was found:\n#{loop}")
        end
        # Should never get here
        raise Sass::SyntaxError.new("An @extend loop exists, but the exact loop couldn't be found")
      end
    end
  end
end
