module Sass::Tree
  # A static node reprenting a CSS property.
  #
  # @see Sass::Tree
  class PropNode < Node
    # The name of the property,
    # interspersed with {Sass::Script::Node}s
    # representing `#{}`-interpolation.
    # Any adjacent strings will be merged together.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :name

    # The name of the property
    # after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Node#perform} has been called.
    #
    # @return [String]
    attr_accessor :resolved_name

    # The value of the property.
    #
    # @return [Sass::Script::Node]
    attr_accessor :value

    # The value of the property
    # after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Node#perform} has been called.
    #
    # @return [String]
    attr_accessor :resolved_value

    # How deep this property is indented
    # relative to a normal property.
    # This is only greater than 0 in the case that:
    #
    # * This node is in a CSS tree
    # * The style is :nested
    # * This is a child property of another property
    # * The parent property has a value, and thus will be rendered
    #
    # @return [Fixnum]
    attr_accessor :tabs

    # @param name [Array<String, Sass::Script::Node>] See \{#name}
    # @param value [Sass::Script::Node] See \{#value}
    # @param prop_syntax [Symbol] `:new` if this property uses `a: b`-style syntax,
    #   `:old` if it uses `:a b`-style syntax
    def initialize(name, value, prop_syntax)
      @name = Haml::Util.strip_string_array(
        Haml::Util.merge_adjacent_strings(name))
      @value = value
      @tabs = 0
      @prop_syntax = prop_syntax
      super()
    end

    # Compares the names and values of two properties.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this node and the other object
    #   are the same
    def ==(other)
      self.class == other.class && name == other.name && value == other.value && super
    end

    # Returns a appropriate message indicating how to escape pseudo-class selectors.
    # This only applies for old-style properties with no value,
    # so returns the empty string if this is new-style.
    #
    # @return [String] The message
    def pseudo_class_selector_message
      return "" if @prop_syntax == :new || !value.is_a?(Sass::Script::String) || !value.value.empty?
      "\nIf #{declaration.dump} should be a selector, use \"\\#{declaration}\" instead."
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      res = declaration(tabs, opts, fmt)
      return res + "#{semi fmt}\n" if children.empty?
      res + children_to_src(tabs, opts, fmt).rstrip + semi(fmt) + "\n"
    end

    # Computes the CSS for the property.
    #
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String] The resulting CSS
    def _to_s(tabs)
      to_return = '  ' * (tabs - 1 + self.tabs) + resolved_name + ":" +
        (style == :compressed ? '' : ' ') + resolved_value + (style == :compressed ? "" : ";")
    end

    # Converts nested properties into flat properties.
    #
    # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
    #   The extensions defined for this tree
    # @param parent [PropNode, nil] The parent node of this node,
    #   or nil if the parent isn't a {PropNode}
    # @raise [Sass::SyntaxError] if the property uses invalid syntax
    def _cssize(extends, parent)
      node = super
      result = node.children.dup
      if !node.resolved_value.empty? || node.children.empty?
        node.send(:check!)
        result.unshift(node)
      end
      result
    end

    # Updates the name and indentation of this node based on the parent name
    # and nesting level.
    #
    # @param extends [Haml::Util::SubsetMap{Selector::Simple => Selector::Sequence}]
    #   The extensions defined for this tree
    # @param parent [PropNode, nil] The parent node of this node,
    #   or nil if the parent isn't a {PropNode}
    def cssize!(extends, parent)
      self.resolved_name = "#{parent.resolved_name}-#{resolved_name}" if parent
      self.tabs = parent.tabs + (parent.resolved_value.empty? ? 0 : 1) if parent && style == :nested
      super
    end

    # Runs any SassScript that may be embedded in the property,
    # and invludes the parent property, if any.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    def perform!(environment)
      @resolved_name = run_interp(@name, environment)
      val = @value.perform(environment)
      @resolved_value =
        if @value.context == :equals && val.is_a?(Sass::Script::String)
          val.value
        else
          val.to_s
        end
      super
    end

    # Returns an error message if the given child node is invalid,
    # and false otherwise.
    #
    # {PropNode} only allows other {PropNode}s and {CommentNode}s as children.
    # @param child [Tree::Node] A potential child node
    # @return [String] An error message if the child is invalid, or nil otherwise
    def invalid_child?(child)
      if !child.is_a?(PropNode) && !child.is_a?(CommentNode)
        "Illegal nesting: Only properties may be nested beneath properties."
      end
    end

    private

    def check!
      if @options[:property_syntax] == :old && @prop_syntax == :new
        raise Sass::SyntaxError.new("Illegal property syntax: can't use new syntax when :property_syntax => :old is set.")
      elsif @options[:property_syntax] == :new && @prop_syntax == :old
        raise Sass::SyntaxError.new("Illegal property syntax: can't use old syntax when :property_syntax => :new is set.")
      elsif resolved_value.empty?
        raise Sass::SyntaxError.new("Invalid property: #{declaration.dump} (no value)." +
          pseudo_class_selector_message)
      end
    end

    def declaration(tabs = 0, opts = {:old => @prop_syntax == :old}, fmt = :sass)
      name = self.name.map {|n| n.is_a?(String) ? n : "\#{#{n.to_sass(opts)}}"}.join
      if name[0] == ?:
        raise Sass::SyntaxError.new("The \":#{name}: #{self.class.val_to_sass(value, opts)}\" hack is not allowed in the Sass indented syntax")
      end

      old = opts[:old] && fmt == :sass
      initial = old ? ':' : ''
      mid = old ? '' : ':'
      "#{'  ' * tabs}#{initial}#{name}#{mid} #{self.class.val_to_sass(value, opts)}".rstrip
    end

    class << self
      # @private
      def val_to_sass(value, opts)
        val_to_sass_comma(value, opts).to_sass(opts)
      end

      private

      def val_to_sass_comma(node, opts)
        return node unless node.is_a?(Sass::Script::Operation)
        return val_to_sass_concat(node, opts) unless node.operator == :comma

        Sass::Script::Operation.new(
          val_to_sass_concat(node.operand1, opts),
          val_to_sass_comma(node.operand2, opts),
          node.operator)
      end

      def val_to_sass_concat(node, opts)
        return node unless node.is_a?(Sass::Script::Operation)
        return val_to_sass_div(node, opts) unless node.operator == :concat

        Sass::Script::Operation.new(
          val_to_sass_div(node.operand1, opts),
          val_to_sass_concat(node.operand2, opts),
          node.operator)
      end

      def val_to_sass_div(node, opts)
        unless node.is_a?(Sass::Script::Operation) && node.operator == :div &&
            node.operand1.is_a?(Sass::Script::Number) &&
            node.operand2.is_a?(Sass::Script::Number) &&
            (node.context == :equals || !node.operand1.original || !node.operand2.original)
          return node
        end

        Sass::Script::String.new("(#{node.to_sass(opts)})")
      end

    end
  end
end
