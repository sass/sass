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

      # Non-destrucively extends this selector with the extensions specified in a hash
      # (which should come from {Sass::Tree::Visitors::Cssize}).
      #
      # @overload def do_extend(extends, parent_directives)
      # @param extends [{Selector::Simple =>
      #                  Sass::Tree::Visitors::Cssize::Extend}]
      #   The extensions to perform on this selector
      # @param parent_directives [Array<Sass::Tree::DirectiveNode>]
      #   The directives containing this selector.
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      # @see CommaSequence#do_extend
      def do_extend(extends, parent_directives, seen = Set.new)
        extends.get(members.to_set).map do |ex, sels|
          # If A {@extend B} and C {...},
          # ex.extender is A, sels is B, and self is C

          self_without_sel = self.members - sels
          next unless unified = ex.extender.members.last.unify(self_without_sel)
          next unless check_directives_match!(ex, parent_directives)
          [sels, ex.extender.members[0...-1] + [unified]]
        end.compact.map do |sels, seq|
          seq = Sequence.new(seq)
          next [] if seen.include?(sels)
          seq.do_extend(extends, parent_directives, seen + [sels])
        end.flatten.uniq
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

      # Returns whether or not this selector matches all elements
      # that the given selector matches (as well as possibly more).
      #
      # @example
      #   (.foo).superselector?(.foo.bar) #=> true
      #   (.foo).superselector?(.bar) #=> false
      # @param sseq [SimpleSequence]
      # @return [Boolean]
      def superselector?(sseq)
        (base.nil? || base.eql?(sseq.base)) && rest.subset?(sseq.rest)
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

      private

      def check_directives_match!(extend, parent_directives)
        dirs1 = extend.directives.map {|d| d.value}
        dirs2 = parent_directives.map {|d| d.value}
        return true if Sass::Util.subsequence?(dirs1, dirs2)

        Sass::Util.sass_warn <<WARNING
DEPRECATION WARNING on line #{extend.node.line}#{" of #{extend.node.filename}" if extend.node.filename}:
  @extending an outer selector from within #{extend.directives.last.name} is deprecated.
  You may only @extend selectors within the same directive.
  This will be an error in Sass 3.3.
  It can only work once @extend is supported natively in the browser.
WARNING
        return false
      end

      def _hash
        [base, Sass::Util.set_hash(rest)].hash
      end

      def _eql?(other)
        other.base.eql?(self.base) && Sass::Util.set_eql?(other.rest, self.rest)
      end
    end
  end
end
