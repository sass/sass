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
      # This is populated during the {Sequence#do_extend} process.
      #
      # @return {Set<Sequence>}
      attr_accessor :sources

      # This sequence source range.
      #
      # @return [Sass::Source::Range]
      attr_accessor :source_range

      # @see \{#subject?}
      attr_writer :subject

      # Returns the element or universal selector in this sequence,
      # if it exists.
      #
      # @return [Element, Universal, nil]
      def base
        @base ||= (members.first if members.first.is_a?(Element) || members.first.is_a?(Universal))
      end

      def pseudo_elements
        @pseudo_elements ||= (members - [base]).
          select {|sel| sel.is_a?(Pseudo) && sel.type == :element}
      end

      # Returns the non-base, non-pseudo-class selectors in this sequence.
      #
      # @return [Set<Simple>]
      def rest
        @rest ||= Set.new(members - [base] - pseudo_elements)
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
      # @param source_range [Sass::Source::Range]
      def initialize(selectors, subject, source_range = nil)
        @members = selectors
        @subject = subject
        @sources = Set.new
        @source_range = source_range
      end

      # Resolves the {Parent} selectors within this selector
      # by replacing them with the given parent selector,
      # handling commas appropriately.
      #
      # @param super_cseq [CommaSequence] The parent selector
      # @return [CommaSequence] This selector, with parent references resolved
      # @raise [Sass::SyntaxError] If a parent selector is invalid
      def resolve_parent_refs(super_cseq)
        # Parent selector only appears as the first selector in the sequence
        unless (parent = @members.first).is_a?(Parent)
          return CommaSequence.new([Sequence.new([self])])
        end

        return super_cseq if @members.size == 1 && parent.suffix.empty?

        CommaSequence.new(super_cseq.members.map do |super_seq|
          members = super_seq.members.dup
          newline = members.pop if members.last == "\n"
          unless members.last.is_a?(SimpleSequence)
            raise Sass::SyntaxError.new("Invalid parent selector for \"#{self}\": \"" +
              super_seq.to_a.join + '"')
          end

          parent_sub = members.last.members
          unless parent.suffix.empty?
            parent_sub = parent_sub.dup
            parent_sub[-1] = parent_sub.last.dup
            case parent_sub.last
            when Sass::Selector::Class, Sass::Selector::Id, Sass::Selector::Placeholder
              parent_sub[-1] = parent_sub.last.class.new(parent_sub.last.name + parent.suffix)
            when Sass::Selector::Element
              parent_sub[-1] = parent_sub.last.class.new(
                parent_sub.last.name + parent.suffix,
                parent_sub.last.namespace)
            when Sass::Selector::Pseudo
              if parent_sub.last.arg
                raise Sass::SyntaxError.new("Invalid parent selector for \"#{self}\": \"" +
                  super_seq.to_a.join + '"')
              end
              parent_sub[-1] = parent_sub.last.class.new(
                parent_sub.last.type,
                parent_sub.last.name + parent.suffix,
                nil)
            else
              raise Sass::SyntaxError.new("Invalid parent selector for \"#{self}\": \"" +
                super_seq.to_a.join + '"')
            end
          end

          Sequence.new(members[0...-1] +
            [SimpleSequence.new(parent_sub + @members[1..-1], subject?)] +
            [newline].compact)
        end)
      end

      # Non-destructively extends this selector with the extensions specified in a hash
      # (which should come from {Sass::Tree::Visitors::Cssize}).
      #
      # @overload do_extend(extends, parent_directives)
      #   @param extends [{Selector::Simple =>
      #                    Sass::Tree::Visitors::Cssize::Extend}]
      #     The extensions to perform on this selector
      #   @param parent_directives [Array<Sass::Tree::DirectiveNode>]
      #     The directives containing this selector.
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      # @see CommaSequence#do_extend
      def do_extend(extends, parent_directives, seen = Set.new)
        groups = Sass::Util.group_by_to_a(extends[members.to_set]) {|ex| ex.extender}
        groups.map! do |seq, group|
          sels = group.map {|e| e.target}.flatten
          # If A {@extend B} and C {...},
          # seq is A, sels is B, and self is C

          self_without_sel = Sass::Util.array_minus(members, sels)
          group.each {|e| e.result = :failed_to_unify unless e.result == :succeeded}
          unified = seq.members.last.unify(self_without_sel, subject?)
          next unless unified
          group.each {|e| e.result = :succeeded}
          group.each {|e| check_directives_match!(e, parent_directives)}
          new_seq = Sequence.new(seq.members[0...-1] + [unified])
          new_seq.add_sources!(sources + [seq])
          [sels, new_seq]
        end
        groups.compact!
        groups.map! do |sels, seq|
          seen.include?(sels) ? [] : seq.do_extend(extends, parent_directives, seen + [sels])
        end
        groups.flatten!
        groups.uniq!
        groups
      end

      # Unifies this selector with another {SimpleSequence}'s
      # {SimpleSequence#members members array}, returning another `SimpleSequence`
      # that matches both this selector and the input selector.
      #
      # @param sels [Array<Simple>] A {SimpleSequence}'s {SimpleSequence#members members array}
      # @param other_subject [Boolean] Whether the other {SimpleSequence} being merged is a subject.
      # @return [SimpleSequence, nil] A {SimpleSequence} matching both `sels` and this selector,
      #   or `nil` if this is impossible (e.g. unifying `#foo` and `#bar`)
      # @raise [Sass::SyntaxError] If this selector cannot be unified.
      #   This will only ever occur when a dynamic selector,
      #   such as {Parent} or {Interpolation}, is used in unification.
      #   Since these selectors should be resolved
      #   by the time extension and unification happen,
      #   this exception will only ever be raised as a result of programmer error
      def unify(sels, other_subject)
        sseq = members.inject(sels) do |member, sel|
          return unless member
          sel.unify(member)
        end
        return unless sseq
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
        (base.nil? || base.eql?(sseq.base)) &&
          pseudo_elements.eql?(sseq.pseudo_elements) &&
          rest.subset?(sseq.rest)
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
      # {SimpleSequence#sources} set.
      #
      # @param sources [Set<Sequence>]
      # @return [SimpleSequence]
      def with_more_sources(sources)
        sseq = dup
        sseq.members = members.dup
        sseq.sources = self.sources | sources
        sseq
      end

      private

      def check_directives_match!(extend, parent_directives)
        dirs1 = extend.directives.map {|d| d.resolved_value}
        dirs2 = parent_directives.map {|d| d.resolved_value}
        return if Sass::Util.subsequence?(dirs1, dirs2)
        line = extend.node.line
        filename = extend.node.filename

        # TODO(nweiz): this should use the Sass stack trace of the extend node,
        # not the selector.
        raise Sass::SyntaxError.new(<<MESSAGE)
You may not @extend an outer selector from within #{extend.directives.last.name}.
You may only @extend selectors within the same directive.
From "@extend #{extend.target.join(', ')}" on line #{line}#{" of #{filename}" if filename}.
MESSAGE
      end

      def _hash
        [base, Sass::Util.set_hash(rest)].hash
      end

      def _eql?(other)
        other.base.eql?(base) && other.pseudo_elements == pseudo_elements &&
          Sass::Util.set_eql?(other.rest, rest) && other.subject? == subject?
      end
    end
  end
end
