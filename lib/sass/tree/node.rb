module Sass
  # A namespace for nodes in the Sass parse tree.
  #
  # The Sass parse tree has two states.
  # When it's first parsed, it has nodes for mixin definitions
  # and for loops and so forth,
  # in addition to nodes for CSS rules and properties.
  #
  # However, {Tree::Node#perform} returns a different sort of tree.
  # This tree maps more closely to the resulting CSS document
  # than it does to the original Sass document.
  # It still has nodes for CSS rules and properties,
  # but it doesn't have any dynamic-generation-related nodes.
  #
  # Nodes that only appear in the pre-perform state are called **dynamic nodes**;
  # those that appear in both states are called **static nodes**.
  module Tree
    # The abstract superclass of all parse-tree nodes.
    class Node
      # The child nodes of this node.
      #
      # @return [Array<Tree::Node>]
      attr_accessor :children

      # The line of the document on which this node appeared.
      #
      # @return [Fixnum]
      attr_accessor :line

      # The name of the document on which this node appeared.
      #
      # @return [String]
      attr_writer :filename

      # The options hash for the node.
      # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
      #
      # @return [Hash<Symbol, Object>]
      attr_reader :options

      def initialize
        @children = []
      end

      # Sets the options hash for the node and all its children.
      #
      # @param options [Hash<Symbol, Object>] The options
      # @see #options
      def options=(options)
        children.each {|c| c.options = options}
        @options = options
      end

      # The name of the document on which this node appeared.
      #
      # @return [String]
      def filename
        @filename || @options[:filename]
      end

      # Appends a child to the node.
      #
      # @param child [Tree::Node] The child node
      # @raise [Sass::SyntaxError] if `child` is invalid
      # @see #invalid_child?
      def <<(child)
        if msg = invalid_child?(child)
          raise Sass::SyntaxError.new(msg, :line => child.line)
        end
        @children << child
      end

      # Return the last child node.
      #
      # We need this because {Tree::Node} duck types as an Array for {Sass::Engine}.
      #
      # @return [Tree::Node] The last child node
      def last
        children.last
      end

      # Compares this node and another object (only other {Tree::Node}s will be equal).
      # This does a structural comparison;
      # if the contents of the nodes and all the child nodes are equivalent,
      # then the nodes are as well.
      #
      # Only static nodes need to override this.
      #
      # @param other [Object] The object to compare with
      # @return [Boolean] Whether or not this node and the other object
      #   are the same
      # @see Sass::Tree
      def ==(other)
        self.class == other.class && other.children == children
      end

      # Runs the dynamic Sass code *and* computes the CSS for the tree.
      #
      # @see #perform
      # @see #to_s
      def render
        perform(Environment.new).to_s
      end

      # True if \{#to\_s} will return `nil`;
      # that is, if the node shouldn't be rendered.
      # Should only be called in a static tree.
      #
      # @return [Boolean]
      def invisible?; false; end

      # The output style. See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
      #
      # @return [Symbol]
      def style
        @options[:style]
      end

      # Computes the CSS corresponding to this Sass tree.
      #
      # Only static-node subclasses need to implement \{#to\_s}.
      #
      # This may return `nil`, but it will only do so if \{#invisible?} is true.
      #
      # @param args [Array] Passed on to \{#\_to\_s}
      # @return [String, nil] The resulting CSS
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see Sass::Tree
      def to_s(*args)
        _to_s(*args)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      # Runs the dynamic Sass code:
      # mixins, variables, control directives, and so forth.
      # This doesn't modify this node or any of its children.
      #
      # \{#perform} shouldn't be overridden directly;
      # if you want to return a new node (or list of nodes),
      # override \{#\_perform};
      # if you want to destructively modify this node,
      # override \{#perform!}.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [Tree::Node] The resulting tree of static nodes
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see Sass::Tree
      def perform(environment)
        environment.options = @options if self.class == Tree::Node
        _perform(environment)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      protected

      # Computes the CSS corresponding to this particular Sass node.
      #
      # @param args [Array] ignored
      # @return [String, nil] The resulting CSS
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see #to_s
      # @see Sass::Tree
      def _to_s
        raise NotImplementedError.new("All static-node subclasses of Sass::Tree::Node must override #_to_s or #to_s.")
      end

      # Runs any dynamic Sass code in this particular node.
      # This doesn't modify this node or any of its children.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [Tree::Node, Array<Tree::Node>] The resulting static nodes
      # @see #perform
      # @see Sass::Tree
      def _perform(environment)
        node = dup
        node.perform!(environment)
        node
      end

      # Destructively runs dynamic Sass code in this particular node.
      # This *does* modify this node,
      # but will be run non-destructively by \{#\_perform\}.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @see #perform
      def perform!(environment)
        self.children = perform_children(Environment.new(environment))
      end

      # Non-destructively runs \{#perform} on all children of the current node.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [Array<Tree::Node>] The resulting static nodes
      def perform_children(environment)
        children.map {|c| c.perform(environment)}.flatten
      end

      # Replaces SassScript in a chunk of text (via `#{}`)
      # with the resulting value.
      #
      # @param text [String] The text to interpolate
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [String] The interpolated text
      def interpolate(text, environment)
        res = ''
        rest = Haml::Shared.handle_interpolation text do |scan|
          escapes = scan[2].size
          res << scan.matched[0...-2 - escapes]
          if escapes % 2 == 1
            res << "\\" * (escapes - 1) << '#{'
          else
            res << "\\" * [0, escapes - 1].max
            res << Script::Parser.new(scan, line, scan.pos - scan.matched_size, filename).
              parse_interpolated.perform(environment).to_s
          end
        end
        res + rest
      end

      # @see Haml::Shared.balance
      # @raise [Sass::SyntaxError] if the brackets aren't balanced
      def balance(*args)
        res = Haml::Shared.balance(*args)
        return res if res
        raise Sass::SyntaxError.new("Unbalanced brackets.", :line => line)
      end

      # Returns an error message if the given child node is invalid,
      # and false otherwise.
      #
      # By default, all child nodes are valid.
      # This is expected to be overriden by subclasses
      # for which some children are invalid.
      #
      # @param child [Tree::Node] A potential child node
      # @return [Boolean, String] Whether or not the child node is valid,
      #   as well as the error message to display if it is invalid
      def invalid_child?(child)
        false
      end
    end
  end
end
