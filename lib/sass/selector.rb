module Sass
  # A namespace for nodes in the parse tree for selectors.
  module Selector
    # The abstract superclass for simple selectors
    # (that is, those that don't compose multiple selectors).
    class Node
      # The line of the Sass template on which this selector was declared.
      #
      # @return [Fixnum]
      attr_accessor :line

      # The name of the file in which this selector was declared,
      # or `nil` if it was not declared in a file (e.g. on stdin).
      #
      # @return [String, nil]
      attr_accessor :filename

      # Returns a representation of the node
      # as an array of strings and potentially {Sass::Script::Node}s
      # (if there's interpolation in the selector).
      # When the interpolation is resolved and the strings are joined together,
      # this will be the string representation of this node.
      #
      # @return [Array<String, Sass::Script::Node>]
      def to_a
        raise NotImplementedError.new("All subclasses of Sass::Selector::Node must override #to_a.")
      end

      # Returns a string representation of the node.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        to_a.map {|e| e.is_a?(Sass::Script::Node) ? "\#{#{e.to_sass}}" : e}.join
      end

      # Returns a hash code for this selector object.
      #
      # By default, this is based on the value of \{#to\_a},
      # so if that contains information irrelevant to the identity of the selector,
      # this should be overridden.
      #
      # @return [Fixnum]
      def hash
        to_a.hash
      end

      # Checks equality between this and another object.
      #
      # By default, this is based on the value of \{#to\_a},
      # so if that contains information irrelevant to the identity of the selector,
      # this should be overridden.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class && other.to_a.eql?(to_a)
      end

      # Unifies this selector with a {SimpleSequence}'s {SimpleSequence#members members array},
      # returning another `SimpleSequence` members array
      # that matches both this selector and the input selector.
      #
      # By default, this just appends this selector to the end of the array
      # (or returns the original array if this selector already exists in it).
      #
      # @param sels [Array<Node>] A {SimpleSequence}'s {SimpleSequence#members members array}
      # @return [Array<Node>, nil] A {SimpleSequence} {SimpleSequence#members members array}
      #   matching both `sels` and this selector,
      #   or `nil` if this is impossible (e.g. unifying `#foo` and `#bar`)
      # @raise [Sass::SyntaxError] If this selector cannot be unified.
      #   This will only ever occur when a dynamic selector,
      #   such as {Parent} or {Interpolation}, is used in unification.
      #   Since these selectors should be resolved
      #   by the time extension and unification happen,
      #   this exception will only ever be raised as a result of programmer error
      def unify(sels)
        return sels if sels.any? {|sel2| eql?(sel2)}
        sels + [self]
      end

      protected

      # Unifies two namespaces,
      # returning a namespace that works for both of them if possible.
      #
      # @param ns1 [String, nil] The first namespace.
      #   `nil` means none specified, e.g. `foo`.
      #   The empty string means no namespace specified, e.g. `|foo`.
      #   `"*"` means any namespace is allowed, e.g. `*|foo`.
      # @param ns2 [String, nil] The second namespace. See `ns1`.
      # @return [Array(String or nil, Boolean)]
      #   The first value is the unified namespace, or `nil` for no namespace.
      #   The second value is whether or not a namespace that works for both inputs
      #   could be found at all.
      #   If the second value is `false`, the first should be ignored.
      def unify_namespaces(ns1, ns2)
        return nil, false unless ns1 == ns2 || ns1.nil? || ns1 == '*' || ns2.nil? || ns2 == '*'
        return ns2, true unless ns2.nil? || ns2 == '*'
        return ns1, true
      end
    end

    # The abstract parent class of the various selector sequence classes.
    #
    # All subclasses should implement a `members` method
    # that returns an array of object that respond to `#line=` and `#filename=`.
    class AbstractSequence
      # The line of the Sass template on which this selector was declared.
      #
      # @return [Fixnum]
      attr_reader :line

      # The name of the file in which this selector was declared.
      #
      # @return [String, nil]
      attr_reader :filename

      # Sets the line of the Sass template on which this selector was declared.
      # This also sets the line for all child selectors.
      #
      # @param line [Fixnum]
      # @return [Fixnum]
      def line=(line)
        members.each {|m| m.line = line}
        @line = line
      end

      # Sets the name of the file in which this selector was declared,
      # or `nil` if it was not declared in a file (e.g. on stdin).
      # This also sets the filename for all child selectors.
      #
      # @param filename [String, nil]
      # @return [String, nil]
      def filename=(filename)
        members.each {|m| m.filename = filename}
        @filename = filename
      end
    end

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

      # Non-destrucively extends this selector
      # with the extensions specified in a hash
      # (which should be populated via {Sass::Tree::Node#cssize}).
      #
      # @todo Link this to the reference documentation on `@extend`
      #   when such a thing exists.
      #
      # @param extends [{Selector::Node => Selector::Node}]
      #   The extensions to perform on this selector
      # @return [CommaSequence] A copy of this selector,
      #   with extensions made according to `extends`
      def extend(extends)
        CommaSequence.new(members.map {|seq| seq.extend(extends)}.flatten)
      end

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join(", ")
      end

      # Returns a hash code for this sequence.
      #
      # @return [Fixnum]
      def hash
        members.hash
      end

      # Checks equality between this and another object.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class && other.members.eql?(self.members)
      end
    end

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

      # Non-destrucively extends this selector
      # with the extensions specified in a hash
      # (which should be populated via {Sass::Tree::Node#cssize}).
      #
      # @param extends [{Selector::Node => Selector::Node}]
      #   The extensions to perform on this selector
      # @return [Array<Sequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      #   These correspond to a {CommaSequence}'s {CommaSequence#members members array}.
      # @see CommaSequence#extend
      def extend(extends)
        new_seqs = [self]
        members.each_with_index do |sseq_or_op, i|
          next unless sseq_or_op.is_a?(SimpleSequence)
          new_seqs.concat(sseq_or_op.extend(extends).map do |sseq|
              Sequence.new(members[0...i] + [sseq] + members[i+1..-1])
            end)
        end
        new_seqs
      end

      # @see Node#to_a
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
    end

    # A unseparated sequence of selectors
    # that all apply to a single element.
    # For example, `.foo#bar[attr=baz]` is a simple sequence
    # of the selectors `.foo`, `#bar`, and `[attr=baz]`.
    class SimpleSequence < AbstractSequence
      # The array of individual selectors.
      #
      # @return [Array<Node>]
      attr_reader :members

      # @param selectors [Array<Node>] See \{#members}
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
      # @overload def extend(extends)
      # @param extends [{Selector::Node => Selector::Node}]
      #   The extensions to perform on this selector
      # @return [Array<SimpleSequence>] A list of selectors generated
      #   by extending this selector with `extends`.
      #   Note that these correspond more to a {CommaSequence}'s {CommaSequence#members members array},
      #   than a {Sequence}'s.
      #   Each individual SimpleSequence will be post-processed into a Sequence
      #   by {Sequence#extend}.
      # @see CommaSequence#extend
      def extend(extends, supers = [])
        Haml::Util.enum_with_index(members).map do |sel, i|
          next unless extenders = extends[sel]
          sseq_without_sel = members[0...i] + members[i+1..-1]
          extenders.map do |sel2|
            next unless unified = sel2.unify(sseq_without_sel)
            unified = SimpleSequence.new(unified)
            res = [unified] + unified.extend(extends, supers.push(sel2))
            supers.pop
            res
          end
        end.flatten.compact
      rescue SystemStackError
        handle_extend_loop(supers)
      end

      # @see Node#to_a
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
        members.hash
      end

      # Checks equality between this and another object.
      #
      # @param other [Object] The object to test equality against
      # @return [Boolean] Whether or not this is equal to `other`
      def eql?(other)
        other.class == self.class && other.members.eql?(self.members)
      end

      private

      # Raise a {Sass::SyntaxError} describing a loop of `@extend` directives.
      #
      # @param supers [Array<Node>] The stack of selectors that contains the loop,
      #   ordered from deepest to most shallow.
      # @raise [Sass::SyntaxError] Describing the loop
      def handle_extend_loop(supers)
        supers.inject([]) do |sels, sel|
          next sels.push(sel) unless sels.first.eql?(sel)
          conses = Haml::Util.enum_cons(sels.push(sel), 2).to_a
          _, i = Haml::Util.enum_with_index(conses).max {|((_, sel1), _), ((_, sel2), _)| sel1.line <=> sel2.line}
          loop = (conses[i..-1] + conses[0...i]).map do |sel1, sel2|
            str = "  #{sel1.inspect} extends #{sel2.inspect} on line #{sel1.line}"
            str << " of " << sel1.filename if sel1.filename
            str
          end.join(",\n")
          raise Sass::SyntaxError.new("An @extend loop was found:\n#{loop}")
        end
        # Should never get here
        raise Sass::SyntaxError.new("An @extend loop exists, but the exact loop couldn't be found")
      end
    end

    # A parent-referencing selector (`&` in Sass).
    # The function of this is to be replaced by the parent selector
    # in the nested hierarchy.
    class Parent < Node
      # @see Node#to_a
      def to_a
        ["&"]
      end

      # Always raises an exception.
      #
      # @raise [Sass::SyntaxError] Parent selectors should be resolved before unification
      # @see Node#unify
      def unify(sels)
        raise Sass::SyntaxError.new("[BUG] Cannot unify parent selectors.")
      end
    end

    # A class selector (e.g. `.foo`).
    class Class < Node
      # The class name.
      #
      # @return [String]
      attr_reader :name

      # @param name [String] The class name
      def initialize(name)
        @name = name
      end

      # @see Node#to_a
      def to_a
        [".", @name]
      end
    end

    # An id selector (e.g. `#foo`).
    class Id < Node
      # The id name.
      #
      # @return [String]
      attr_reader :name

      # @param name [String] The id name
      def initialize(name)
        @name = name
      end

      # @see Node#to_a
      def to_a
        ["#", @name]
      end

      # Returns `nil` if `sels` contains an {Id} selector
      # with a different name than this one.
      #
      # @see Node#unify
      def unify(sels)
        return if sels.any? {|sel2| sel2.is_a?(Id) && self.name != sel2.name}
        super
      end
    end

    # A universal selector (`*` in CSS).
    class Universal < Node
      # The selector namespace.
      # `nil` means the default namespace,
      # `""` means no namespace,
      # `"*"` means any namespace.
      #
      # @return [String, nil]
      attr_reader :namespace

      # @param namespace [String, nil] See \{#namespace}
      def initialize(namespace)
        @namespace = namespace
      end

      # @see Node#to_a
      def to_a
        @namespace ? [@namespace, "|*"] : ["*"]
      end

      # Unification of a universal selector is somewhat complicated,
      # especially when a namespace is specified.
      # If there is no namespace specified
      # or any namespace is specified (namespace `"*"`),
      # then `sel` is returned without change
      # (unless it's empty, in which case `"*"` is required).
      #
      # If a namespace is specified
      # but `sel` does not specify a namespace,
      # then the given namespace is applied to `sel`,
      # either by adding this {Universal} selector
      # or applying this namespace to an existing {Element} selector.
      #
      # If both this selector *and* `sel` specify namespaces,
      # those namespaces are unified via {Node#unify_namespaces}
      # and the unified namespace is used, if possible.
      #
      # @todo There are lots of cases that this documentation specifies;
      #   make sure we thoroughly test **all of them**.
      # @todo Keep track of whether a default namespace has been declared
      #   and handle namespace-unspecified selectors accordingly.
      # @todo If any branch of a CommaSequence ends up being just `"*"`,
      #   then all other branches should be eliminated
      #
      # @see Node#unify
      def unify(sels)
        name =
          case sels.first
          when Universal; :universal
          when Element; sels.first.name
          else
            return [self] + sels unless namespace == nil || namespace == '*'
            return sels unless sels.empty?
            return [self]
          end

        ns, accept = unify_namespaces(namespace, sels.first.namespace)
        return unless accept
        [name == :universal ? Universal.new(ns) : Element.new(name, ns)] + sels[1..-1]
      end
    end

    # An element selector (e.g. `h1`).
    class Element < Node
      # The element name.
      #
      # @return [String]
      attr_reader :name

      # The selector namespace.
      # `nil` means the default namespace,
      # `""` means no namespace,
      # `"*"` means any namespace.
      #
      # @return [String, nil]
      attr_reader :namespace

      # @param name [String] The element name
      # @param namespace [String, nil] See \{#namespace}
      def initialize(name, namespace)
        @name = name
        @namespace = namespace
      end

      # @see Node#to_a
      def to_a
        @namespace ? [@namespace, "|", @name] : [@name]
      end

      # Unification of an element selector is somewhat complicated,
      # especially when a namespace is specified.
      # First, if `sel` contains another {Element} with a different \{#name},
      # then the selectors can't be unified and `nil` is returned.
      #
      # Otherwise, if `sel` doesn't specify a namespace,
      # or it specifies any namespace (via `"*"`),
      # then it's returned with this element selector
      # (e.g. `.foo` becomes `a.foo` or `svg|a.foo`).
      # Similarly, if this selector doesn't specify a namespace,
      # the namespace from `sel` is used.
      #
      # If both this selector *and* `sel` specify namespaces,
      # those namespaces are unified via {Node#unify_namespaces}
      # and the unified namespace is used, if possible.
      #
      # @todo There are lots of cases that this documentation specifies;
      #   make sure we thoroughly test **all of them**.
      # @todo Keep track of whether a default namespace has been declared
      #   and handle namespace-unspecified selectors accordingly.
      #
      # @see Node#unify
      def unify(sels)
        case sels.first
        when Universal;
        when Element; return unless name == sels.first.name
        else return [self] + sels
        end

        ns, accept = unify_namespaces(namespace, sels.first.namespace)
        return unless accept
        [Element.new(name, ns)] + sels[1..-1]
      end
    end

    # Selector interpolation (`#{}` in Sass).
    class Interpolation < Node
      # The script to run.
      #
      # @return [Sass::Script::Node]
      attr_reader :script

      # @param script [Sass::Script::Node] The script to run
      def initialize(script)
        @script = script
      end

      # @see Node#to_a
      def to_a
        [@script]
      end

      # Always raises an exception.
      #
      # @raise [Sass::SyntaxError] Interpolation selectors should be resolved before unification
      # @see Node#unify
      def unify(sels)
        raise Sass::SyntaxError.new("[BUG] Cannot unify interpolation selectors.")
      end
    end

    # An attribute selector (e.g. `[href^="http://"]`).
    class Attribute < Node
      # The attribute name.
      #
      # @return [String]
      attr_reader :name

      # The attribute namespace.
      # `nil` means the default namespace,
      # `""` means no namespace,
      # `"*"` means any namespace.
      #
      # @return [String, nil]
      attr_reader :namespace

      # The matching operator, e.g. `"="` or `"^="`.
      #
      # @return [String]
      attr_reader :operator

      # The right-hand side of the operator.
      #
      # This may include SassScript nodes that will be run during resolution.
      # Note that this should not include SassScript nodes
      # after resolution has taken place.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :value

      # @param name [String] The attribute name
      # @param namespace [String, nil] See \{#namespace}
      # @param operator [String] The matching operator, e.g. `"="` or `"^="`
      # @param value [Array<String, Sass::Script::Node>] See \{#value}
      def initialize(name, namespace, operator, value)
        @name = name
        @namespace = namespace
        @operator = operator
        @value = value
      end

      # @see Node#to_a
      def to_a
        res = ["["]
        res << @namespace << "|" if @namespace
        res << @name
        (res << @operator).concat @value if @value
        res << "]"
      end
    end

    # A pseudoclass (e.g. `:visited`) or pseudoelement (e.g. `::first-line`) selector.
    # It can have arguments (e.g. `:nth-child(2n+1)`).
    class Pseudo < Node
      # The type of the selector.
      # `:class` if this is a pseudoclass selector,
      # `:element` if it's a pseudoelement.
      #
      # @return [Symbol]
      attr_reader :type

      # The name of the selector.
      #
      # @return [String]
      attr_reader :name

      # The argument to the selector,
      # or `nil` if no argument was given.
      #
      # This may include SassScript nodes that will be run during resolution.
      # Note that this should not include SassScript nodes
      # after resolution has taken place.
      #
      # @return [Array<String, Sass::Script::Node>, nil]
      attr_reader :arg

      # @param type [Symbol] See \{#type}
      # @param name [String] The name of the selector
      # @param arg [nil, Array<String, Sass::Script::Node>] The argument to the selector,
      #   or nil if no argument was given
      def initialize(type, name, arg)
        @type = type
        @name = name
        @arg = arg
      end

      # @see Node#to_a
      def to_a
        res = [@type == :class ? ":" : "::", @name]
        (res << "(").concat(@arg) << ")" if @arg
        res
      end
    end

    # A negation pseudoclass selector (e.g. `:not(.foo)`).
    class Negation < Node
      # The selector to negate.
      #
      # @return [Node]
      attr_reader :selector

      # @param [Node] The selector to negate
      def initialize(selector)
        @selector = selector
      end

      # @see Node#to_a
      def to_a
        [":not("] + @selector.to_a + [")"]
      end
    end
  end
end
