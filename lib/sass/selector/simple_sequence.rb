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
      attr_accessor :members

      # The extending selectors that caused this selector sequence to be
      # generated. For example:
      #
      #     a.foo { ... }
      #     b.bar {@extend a}
      #     c.baz {@extend b}
      #
      # The generated selector `b.foo.bar` has `{b.bar}` as its `sources` set,
      # and the generated selector `c.foo.bar.baz` has `{b.bar, c.baz}` as its
      # `sources` set.
      #
      # This is populated during the {#do_extend} process.
      #
      # @return {Set<Sequence>}
      attr_accessor :sources

      # @see \{#subject?}
      attr_writer :subject

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

      # Whether or not this compound selector is the subject of the parent
      # selector; that is, whether it is prepended with `$` and represents the
      # actual element that will be selected.
      #
      # @return [Boolean]
      def subject?
        @subject
      end

      # @param selectors [Array<Simple>] See \{#members}
      # @param subject [Boolean] See \{#subject?}
      # @param sources [Set<Sequence>]
      def initialize(selectors, subject, sources = Set.new)
        @members = selectors
        @subject = subject
        @sources = sources
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
          [SimpleSequence.new(super_seq.members.last.members + @members[1..-1], subject?)]
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
        Sass::Util.group_by_to_a(extends.get(members.to_set)) {|ex, _| ex.extender}.map do |seq, group|
          sels = group.map {|_, s| s}.flatten
          # If A {@extend B} and C {...},
          # seq is A, sels is B, and self is C

          self_without_sel = self.members - sels
          group.each {|e, _| e.result = :failed_to_unify}
          next unless unified = seq.members.last.unify(self_without_sel, subject?)
          group.each {|e, _| e.result = :succeeded}
          next if group.map {|e, _| check_directives_match!(e, parent_directives)}.none?
          new_seq = Sequence.new(seq.members[0...-1] + [unified])
          new_seq.add_sources!(sources + [seq])
          [sels, new_seq]
        end.compact.map do |sels, seq|
          seen.include?(sels) ? [] : seq.do_extend(extends, parent_directives, seen + [sels])
        end.flatten.uniq
      end

      # Unifies this selector with another {SimpleSequence}'s {SimpleSequence#members members array},
      # returning another `SimpleSequence`
      # that matches both this selector and the input selector.
      #
      # @param sels [Array<Simple>] A {SimpleSequence}'s {SimpleSequence#members members array}
      # @param subject [Boolean] Whether the {SimpleSequence} being merged is a subject.
      # @return [SimpleSequence, nil] A {SimpleSequence} matching both `sels` and this selector,
      #   or `nil` if this is impossible (e.g. unifying `#foo` and `#bar`)
      # @raise [Sass::SyntaxError] If this selector cannot be unified.
      #   This will only ever occur when a dynamic selector,
      #   such as {Parent} or {Interpolation}, is used in unification.
      #   Since these selectors should be resolved
      #   by the time extension and unification happen,
      #   this exception will only ever be raised as a result of programmer error
      def unify(sels, other_subject)
        return unless sseq = members.inject(sels) do |sseq, sel|
          return unless sseq
          sel.unify(sseq)
        end
        SimpleSequence.new(sseq, other_subject || subject?)
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
        res = @members.map {|sel| sel.to_a}.flatten
        res << '!' if subject?
        res
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join
      end

      # Return a copy of this simple sequence with `sources` merged into the
      # {#sources} set.
      #
      # @param sources [Set<Sequence>]
      # @return [SimpleSequence]
      def with_more_sources(sources)
        sseq = dup
        sseq.members = members.dup
        sseq.sources.merge sources
        sseq
      end

      private

      def check_directives_match!(extend, parent_directives)
        dirs1 = extend.directives.map {|d| d.resolved_value}
        dirs2 = parent_directives.map {|d| d.resolved_value}
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
        other.base.eql?(self.base) && Sass::Util.set_eql?(other.rest, self.rest) &&
          other.subject? == self.subject?
      end
    end
  end
end
