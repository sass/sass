require 'sass/selector/simple'
require 'sass/selector/abstract_sequence'
require 'sass/selector/comma_sequence'
require 'sass/selector/sequence'
require 'sass/selector/simple_sequence'

module Sass
  # A namespace for nodes in the parse tree for selectors.
  #
  # {CommaSequence} is the toplevel seelctor,
  # representing a comma-separated sequence of {Sequence}s,
  # such as `foo bar, baz bang`.
  # {Sequence} is the next level,
  # representing {SimpleSequence}s separated by combinators (e.g. descendant or child),
  # such as `foo bar` or `foo > bar baz`.
  # {SimpleSequence} is a sequence of selectors that all apply to a single element,
  # such as `foo.bar[attr=val]`.
  # Finally, {Simple} is the superclass of the simplest selectors,
  # such as `.foo` or `#bar`.
  module Selector
    # A parent-referencing selector (`&` in Sass).
    # The function of this is to be replaced by the parent selector
    # in the nested hierarchy.
    class Parent < Simple
      # @see Selector#to_a
      def to_a
        ["&"]
      end

      # Always raises an exception.
      #
      # @raise [Sass::SyntaxError] Parent selectors should be resolved before unification
      # @see Selector#unify
      def unify(sels)
        raise Sass::SyntaxError.new("[BUG] Cannot unify parent selectors.")
      end
    end

    # A class selector (e.g. `.foo`).
    class Class < Simple
      # The class name.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :name

      # @param name [Array<String, Sass::Script::Node>] The class name
      def initialize(name)
        @name = name
      end

      # @see Selector#to_a
      def to_a
        [".", *@name]
      end
    end

    # An id selector (e.g. `#foo`).
    class Id < Simple
      # The id name.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :name

      # @param name [Array<String, Sass::Script::Node>] The id name
      def initialize(name)
        @name = name
      end

      # @see Selector#to_a
      def to_a
        ["#", *@name]
      end

      # Returns `nil` if `sels` contains an {Id} selector
      # with a different name than this one.
      #
      # @see Selector#unify
      def unify(sels)
        return if sels.any? {|sel2| sel2.is_a?(Id) && self.name != sel2.name}
        super
      end
    end

    # A universal selector (`*` in CSS).
    class Universal < Simple
      # The selector namespace.
      # `nil` means the default namespace,
      # `[""]` means no namespace,
      # `["*"]` means any namespace.
      #
      # @return [Array<String, Sass::Script::Node>, nil]
      attr_reader :namespace

      # @param namespace [Array<String, Sass::Script::Node>, nil] See \{#namespace}
      def initialize(namespace)
        @namespace = namespace
      end

      # @see Selector#to_a
      def to_a
        @namespace ? @namespace + ["|*"] : ["*"]
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
      # those namespaces are unified via {Simple#unify_namespaces}
      # and the unified namespace is used, if possible.
      #
      # @todo There are lots of cases that this documentation specifies;
      #   make sure we thoroughly test **all of them**.
      # @todo Keep track of whether a default namespace has been declared
      #   and handle namespace-unspecified selectors accordingly.
      # @todo If any branch of a CommaSequence ends up being just `"*"`,
      #   then all other branches should be eliminated
      #
      # @see Selector#unify
      def unify(sels)
        name =
          case sels.first
          when Universal; :universal
          when Element; sels.first.name
          else
            return [self] + sels unless namespace.nil? || namespace == ['*']
            return sels unless sels.empty?
            return [self]
          end

        ns, accept = unify_namespaces(namespace, sels.first.namespace)
        return unless accept
        [name == :universal ? Universal.new(ns) : Element.new(name, ns)] + sels[1..-1]
      end
    end

    # An element selector (e.g. `h1`).
    class Element < Simple
      # The element name.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :name

      # The selector namespace.
      # `nil` means the default namespace,
      # `[""]` means no namespace,
      # `["*"]` means any namespace.
      #
      # @return [Array<String, Sass::Script::Node>, nil]
      attr_reader :namespace

      # @param name [Array<String, Sass::Script::Node>] The element name
      # @param namespace [Array<String, Sass::Script::Node>, nil] See \{#namespace}
      def initialize(name, namespace)
        @name = name
        @namespace = namespace
      end

      # @see Selector#to_a
      def to_a
        @namespace ? @namespace + ["|"] + @name : @name
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
      # those namespaces are unified via {Simple#unify_namespaces}
      # and the unified namespace is used, if possible.
      #
      # @todo There are lots of cases that this documentation specifies;
      #   make sure we thoroughly test **all of them**.
      # @todo Keep track of whether a default namespace has been declared
      #   and handle namespace-unspecified selectors accordingly.
      #
      # @see Selector#unify
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
    class Interpolation < Simple
      # The script to run.
      #
      # @return [Sass::Script::Node]
      attr_reader :script

      # @param script [Sass::Script::Node] The script to run
      def initialize(script)
        @script = script
      end

      # @see Selector#to_a
      def to_a
        [@script]
      end

      # Always raises an exception.
      #
      # @raise [Sass::SyntaxError] Interpolation selectors should be resolved before unification
      # @see Selector#unify
      def unify(sels)
        raise Sass::SyntaxError.new("[BUG] Cannot unify interpolation selectors.")
      end
    end

    # An attribute selector (e.g. `[href^="http://"]`).
    class Attribute < Simple
      # The attribute name.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :name

      # The attribute namespace.
      # `nil` means the default namespace,
      # `[""]` means no namespace,
      # `["*"]` means any namespace.
      #
      # @return [Array<String, Sass::Script::Node>, nil]
      attr_reader :namespace

      # The matching operator, e.g. `"="` or `"^="`.
      #
      # @return [String]
      attr_reader :operator

      # The right-hand side of the operator.
      #
      # @return [Array<String, Sass::Script::Node>]
      attr_reader :value

      # @param name [Array<String, Sass::Script::Node>] The attribute name
      # @param namespace [Array<String, Sass::Script::Node>, nil] See \{#namespace}
      # @param operator [String] The matching operator, e.g. `"="` or `"^="`
      # @param value [Array<String, Sass::Script::Node>] See \{#value}
      def initialize(name, namespace, operator, value)
        @name = name
        @namespace = namespace
        @operator = operator
        @value = value
      end

      # @see Selector#to_a
      def to_a
        res = ["["]
        res.concat(@namespace) << "|" if @namespace
        res.concat @name
        (res << @operator).concat @value if @value
        res << "]"
      end
    end

    # A pseudoclass (e.g. `:visited`) or pseudoelement (e.g. `::first-line`) selector.
    # It can have arguments (e.g. `:nth-child(2n+1)`).
    class Pseudo < Simple
      # The type of the selector.
      # `:class` if this is a pseudoclass selector,
      # `:element` if it's a pseudoelement.
      #
      # @return [Symbol]
      attr_reader :type

      # The name of the selector.
      #
      # @return [Array<String, Sass::Script::Node>]
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
      # @param name [Array<String, Sass::Script::Node>] The name of the selector
      # @param arg [nil, Array<String, Sass::Script::Node>] The argument to the selector,
      #   or nil if no argument was given
      def initialize(type, name, arg)
        @type = type
        @name = name
        @arg = arg
      end

      # @see Selector#to_a
      def to_a
        res = [@type == :class ? ":" : "::"] + @name
        (res << "(").concat(Sass::Util.strip_string_array(@arg)) << ")" if @arg
        res
      end

      # Returns `nil` if this is a pseudoclass selector
      # and `sels` contains a pseudoclass selector different than this one.
      #
      # @see Selector#unify
      def unify(sels)
        return if type == :element && sels.any? do |sel|
          sel.is_a?(Pseudo) && sel.type == :element &&
            (sel.name != self.name || sel.arg != self.arg)
        end
        super
      end
    end

    # A pseudoclass selector whose argument is itself a selector
    # (e.g. `:not(.foo)` or `:-moz-all(.foo, .bar)`).
    class SelectorPseudoClass < Simple
      # The name of the pseudoclass.
      #
      # @return [String]
      attr_reader :name

      # The selector argument.
      #
      # @return [Selector::Sequence]
      attr_reader :selector

      # @param [String] The name of the pseudoclass
      # @param [Selector::Sequence] The selector argument
      def initialize(name, selector)
        @name = name
        @selector = selector
      end

      # @see Selector#to_a
      def to_a
        [":", @name, "("] + @selector.to_a + [")"]
      end
    end
  end
end
