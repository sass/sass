module Sass
  # A namespace for nodes in the parse tree for selectors.
  module Selector
    # The abstract superclass for simple selectors
    # (that is, those that don't compose multiple selectors).
    class Node
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
    end

    # A comma-separated sequence of selectors.
    class CommaSequence
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

      # Returns a string representation of the sequence.
      # This is basically the selector string.
      #
      # @return [String]
      def inspect
        members.map {|m| m.inspect}.join(", ")
      end
    end

    # An operator-separated sequence of
    # {SimpleSequence simple selector sequences}.
    class Sequence
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
    end

    # A unseparated sequence of selectors
    # that all apply to a single element.
    # For example, `.foo#bar[attr=baz]` is a simple sequence
    # of the selectors `.foo`, `#bar`, and `[attr=baz]`.
    class SimpleSequence
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
    end

    # A parent-referencing selector (`&` in Sass).
    # The function of this is to be replaced by the parent selector
    # in the nested hierarchy.
    class Parent < Node
      # @see Node#to_a
      def to_a
        ["&"]
      end
    end

    # A class selector (e.g. `.foo`).
    class Class < Node
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
      # @param name [String] The id name
      def initialize(name)
        @name = name
      end

      # @see Node#to_a
      def to_a
        ["#", @name]
      end
    end

    # A universal selector (`*` in CSS).
    class Universal < Node
      # @param namespace [String, nil] The namespace of the universal selector.
      #   `nil` means the default namespace, `""` means no namespace
      def initialize(namespace)
        @namespace = namespace
      end

      # @see Node#to_a
      def to_a
        @namespace ? [@namespace, "|*"] : ["*"]
      end
    end

    # An element selector (e.g. `h1`).
    class Element < Node
      # @param name [String] The element name
      # @param namespace [String, nil] The namespace of the universal selector.
      #   `nil` means the default namespace, `""` means no namespace
      def initialize(name, namespace)
        @name = name
        @namespace = namespace
      end

      # @see Node#to_a
      def to_a
        @namespace ? [@namespace, "|", @name] : [@name]
      end
    end

    # Selector interpolation (`#{}` in Sass).
    class Interpolation < Node
      # @param script [Sass::Script::Node] The script to run
      def initialize(script)
        @script = script
      end

      # @see Node#to_a
      def to_a
        [@script]
      end
    end

    # An attribute selector (e.g. `[href^="http://"]`).
    class Attribute < Node
      # @param name [String] The attribute name
      # @param namespace [String, nil] The namespace of the universal selector.
      #   `nil` means the default namespace, `""` means no namespace
      # @param operator [String] The matching operator, e.g. `"="` or `"^="`
      # @param value [Array<String, Sass::Script::Node>] The left-hand side of the operator
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
      # @param type [Symbol] `:class` if this is a pseudoclass,
      #   `:element` if this is a pseudoelement
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
