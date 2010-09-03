module Sass
  # A namespace for nodes in the Sass parse tree.
  #
  # The Sass parse tree has three states: dynamic, static Sass, and static CSS.
  #
  # When it's first parsed, a Sass document is in the dynamic state.
  # It has nodes for mixin definitions and `@for` loops and so forth,
  # in addition to nodes for CSS rules and properties.
  # Nodes that only appear in this state are called **dynamic nodes**.
  #
  # {Tree::Node#perform} returns a static Sass tree, which is different.
  # It still has nodes for CSS rules and properties
  # but it doesn't have any dynamic-generation-related nodes.
  # The nodes in this state are in the same structure as the Sass document:
  # rules and properties are nested beneath one another.
  # Nodes that can be in this state or in the dynamic state
  # are called **static nodes**.
  #
  # {Tree::Node#cssize} then returns a static CSS tree.
  # This is like a static Sass tree,
  # but the structure exactly mirrors that of the generated CSS.
  # Rules and properties can't be nested beneath one another in this state.
  #
  # Finally, {Tree::Node#to_s} can be called on a static CSS tree
  # to get the actual CSS code as a string.
  module Tree
    # The abstract superclass of all parse-tree nodes.
    class Node
      include Enumerable

      # The child nodes of this node.
      #
      # @return [Array<Tree::Node>]
      attr_accessor :children

      # Whether or not this node has child nodes.
      # This may be true even when \{#children} is empty,
      # in which case this node has an empty block (e.g. `{}`).
      #
      # @return [Boolean]
      attr_accessor :has_children

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
      # @return [{Symbol => Object}]
      attr_reader :options

      def initialize
        @children = []
      end

      # Sets the options hash for the node and all its children.
      #
      # @param options [{Symbol => Object}] The options
      # @see #options
      def options=(options)
        children.each {|c| c.options = options}
        @options = options
      end

      # @private
      def children=(children)
        self.has_children ||= !children.empty?
        @children = children
      end

      # The name of the document on which this node appeared.
      #
      # @return [String]
      def filename
        @filename || (@options && @options[:filename])
      end

      # Appends a child to the node.
      #
      # @param child [Tree::Node, Array<Tree::Node>] The child node or nodes
      # @raise [Sass::SyntaxError] if `child` is invalid
      # @see #invalid_child?
      def <<(child)
        return if child.nil?
        if child.is_a?(Array)
          child.each {|c| self << c}
        else
          check_child! child
          self.has_children = true
          @children << child
        end
      end

      # Raises an error if the given child node is invalid.
      #
      # @param child [Tree::Node] The child node
      # @raise [Sass::SyntaxError] if `child` is invalid
      # @see #invalid_child?
      def check_child!(child)
        if msg = invalid_child?(child)
          raise Sass::SyntaxError.new(msg, :line => child.line)
        end
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

      # Computes the CSS corresponding to this static CSS tree.
      #
      # \{#to_s} shouldn't be overridden directly; instead, override \{#\_to\_s}.
      # Only static-node subclasses need to implement \{#to\_s}.
      #
      # This may return `nil`, but it will only do so if \{#invisible?} is true.
      #
      # @param args [Array] Passed on to \{#\_to\_s}
      # @return [String, nil] The resulting CSS
      # @see Sass::Tree
      def to_s(*args)
        _to_s(*args)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      # Converts a static CSS tree (e.g. the output of \{#cssize})
      # into another static CSS tree,
      # with the given extensions applied to all relevant {RuleNode}s.
      #
      # @todo Link this to the reference documentation on `@extend`
      #   when such a thing exists.
      #
      # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions to perform on this tree
      # @return [Tree::Node] The resulting tree of static CSS nodes.
      # @raise [Sass::SyntaxError] Only if there's a programmer error
      #   and this is not a static CSS tree
      def do_extend(extends)
        node = dup
        node.children = children.map {|c| c.do_extend(extends)}
        node
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      # Converts a static Sass tree (e.g. the output of \{#perform})
      # into a static CSS tree.
      #
      # \{#cssize} shouldn't be overridden directly;
      # instead, override \{#\_cssize} or \{#cssize!}.
      #
      # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions defined for this tree
      # @param parent [Node, nil] The parent node of this node.
      #   This should only be non-nil if the parent is the same class as this node
      # @return [Tree::Node] The resulting tree of static nodes
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see Sass::Tree
      def cssize(extends, parent = nil)
        _cssize(extends, (parent if parent.class == self.class))
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      # Converts a dynamic tree into a static Sass tree.
      # That is, runs the dynamic Sass code:
      # mixins, variables, control directives, and so forth.
      # This doesn't modify this node or any of its children.
      #
      # \{#perform} shouldn't be overridden directly;
      # instead, override \{#\_perform} or \{#perform!}.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [Tree::Node] The resulting tree of static nodes
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see Sass::Tree
      def perform(environment)
        _perform(environment)
      rescue Sass::SyntaxError => e
        e.modify_backtrace(:filename => filename, :line => line)
        raise e
      end

      # Iterates through each node in the tree rooted at this node
      # in a pre-order walk.
      #
      # @yield node
      # @yieldparam node [Node] a node in the tree
      def each(&block)
        yield self
        children.each {|c| c.each(&block)}
      end

      # Converts a node to Sass code that will generate it.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the Sass code
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @return [String] The Sass code corresponding to the node
      def to_sass(tabs = 0, opts = {})
        to_src(tabs, opts, :sass)
      end

      # Converts a node to SCSS code that will generate it.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the SCSS code
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @return [String] The Sass code corresponding to the node
      def to_scss(tabs = 0, opts = {})
        to_src(tabs, opts, :scss)
      end

      protected

      # Computes the CSS corresponding to this particular Sass node.
      #
      # This method should never raise {Sass::SyntaxError}s.
      # Such errors will not be properly annotated with Sass backtrace information.
      # All error conditions should be checked in earlier transformations,
      # such as \{#cssize} and \{#perform}.
      #
      # @param args [Array] ignored
      # @return [String, nil] The resulting CSS
      # @see #to_s
      # @see Sass::Tree
      def _to_s
        raise NotImplementedError.new("All static-node subclasses of Sass::Tree::Node must override #_to_s or #to_s.")
      end

      # Converts this static Sass node into a static CSS node,
      # returning the new node.
      # This doesn't modify this node or any of its children.
      #
      # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions defined for this tree
      # @param parent [Node, nil] The parent node of this node.
      #   This should only be non-nil if the parent is the same class as this node
      # @return [Tree::Node, Array<Tree::Node>] The resulting static CSS nodes
      # @raise [Sass::SyntaxError] if some element of the tree is invalid
      # @see #cssize
      # @see Sass::Tree
      def _cssize(extends, parent)
        node = dup
        node.cssize!(extends, parent)
        node
      end

      # Destructively converts this static Sass node into a static CSS node.
      # This *does* modify this node,
      # but will be run non-destructively by \{#\_cssize\}.
      #
      # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
      #   The extensions defined for this tree
      # @param parent [Node, nil] The parent node of this node.
      #   This should only be non-nil if the parent is the same class as this node
      # @see #cssize
      def cssize!(extends, parent)
        self.children = children.map {|c| c.cssize(extends, self)}.flatten
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

      # Replaces SassScript in a chunk of text
      # with the resulting value.
      #
      # @param text [Array<String, Sass::Script::Node>] The text to interpolate
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      # @return [String] The interpolated text
      def run_interp(text, environment)
        text.map do |r|
          next r if r.is_a?(String)
          val = r.perform(environment)
          # Interpolated strings should never render with quotes
          next val.value if val.is_a?(Sass::Script::String)
          val.to_s
        end.join.strip
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
      # By default, all child nodes except those only allowed under specific nodes
      # ({Tree::MixinDefNode}, {Tree::ImportNode}, {Tree::ExtendNode}) are valid.
      # This is expected to be overriden by subclasses
      # for which some children are invalid.
      #
      # @param child [Tree::Node] A potential child node
      # @return [Boolean, String] Whether or not the child node is valid,
      #   as well as the error message to display if it is invalid
      def invalid_child?(child)
        case child
        when Tree::MixinDefNode
          "Mixins may only be defined at the root of a document."
        when Tree::ImportNode
          "Import directives may only be used at the root of a document."
        when Tree::ExtendNode
          "Extend directives may only be used within rules."
        end
      end

      # Converts a node to Sass or SCSS code that will generate it.
      #
      # This method is called by the default \{#to\_sass} and \{#to\_scss} methods,
      # so that the same code can be used for both with minor variations.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the SCSS code
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @param fmt [Symbol] `:sass` or `:scss`
      # @return [String] The Sass or SCSS code corresponding to the node
      def to_src(tabs, opts, fmt)
        raise NotImplementedError.new("All static-node subclasses of Sass::Tree::Node must override #to_#{fmt}.")
      end

      # Converts the children of this node to a Sass or SCSS string.
      # This will return the trailing newline for the previous line,
      # including brackets if this is SCSS.
      #
      # @param tabs [Fixnum] The amount of tabulation to use for the Sass code
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @param fmt [Symbol] `:sass` or `:scss`
      # @return [String] The Sass or SCSS code corresponding to the children
      def children_to_src(tabs, opts, fmt)
        return fmt == :sass ? "\n" : " {}\n" if children.empty?

        (fmt == :sass ? "\n" : " {\n") +
          children.map {|c| c.send("to_#{fmt}", tabs + 1, opts)}.join.rstrip +
          (fmt == :sass ? "\n" : " }\n")
      end

      # Converts a selector to a Sass or SCSS string.
      #
      # @param sel [Array<String, Sass::Script::Node>] The selector to convert
      # @param tabs [Fixnum] The indentation of the selector
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @param fmt [Symbol] `:sass` or `:scss`
      # @return [String] The Sass or SCSS code corresponding to the selector
      def selector_to_src(sel, tabs, opts, fmt)
        fmt == :sass ? selector_to_sass(sel, opts) : selector_to_scss(sel, tabs, opts)
      end

      # Converts a selector to a Sass string.
      #
      # @param sel [Array<String, Sass::Script::Node>] The selector to convert
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @return [String] The Sass code corresponding to the selector
      def selector_to_sass(sel, opts)
        sel.map do |r|
          if r.is_a?(String)
            r.gsub(/(,[ \t]*)?\n\s*/) {$1 ? $1 + "\n" : " "}
          else
            "\#{#{r.to_sass(opts)}}"
          end
        end.join
      end

      # Converts a selector to a SCSS string.
      #
      # @param sel [Array<String, Sass::Script::Node>] The selector to convert
      # @param tabs [Fixnum] The indentation of the selector
      # @param opts [{Symbol => Object}] An options hash (see {Sass::CSS#initialize})
      # @return [String] The SCSS code corresponding to the selector
      def selector_to_scss(sel, tabs, opts)
        sel.map {|r| r.is_a?(String) ? r : "\#{#{r.to_sass(opts)}}"}.
          join.gsub(/^[ \t]*/, '  ' * tabs)
      end

      # Convert any underscores in a string into hyphens,
      # but only if the `:dasherize` option is set.
      #
      # @param s [String] The string to convert
      # @param opts [{Symbol => Object}] The options hash
      # @return [String] The converted string
      def dasherize(s, opts)
        if opts[:dasherize]
          s.gsub('_', '-')
        else
          s
        end
      end

      # Returns a semicolon if this is SCSS, or an empty string if this is Sass.
      #
      # @param fmt [Symbol] `:sass` or `:scss`
      # @return [String] A semicolon or the empty string
      def semi(fmt)
        fmt == :sass ? "" : ";"
      end
    end
  end
end
