module Sass::Tree
  # A static node reprenting a CSS property.
  #
  # @see Sass::Tree
  class PropNode < Node
    # The name of the property.
    #
    # @return [String]
    attr_accessor :name

    # The value of the property,
    # either a plain string or a SassScript parse tree.
    #
    # @return [String, Script::Node]
    attr_accessor :value

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

    # @param name [String] See \{#name}
    # @param value [String] See \{#value}
    # @param prop_syntax [Symbol] `:new` if this property uses `a: b`-style syntax,
    #   `:old` if it uses `:a b`-style syntax
    def initialize(name, value, prop_syntax)
      @name = name
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
      return "" if @prop_syntax == :new || !value.empty?
      "\nIf #{declaration.dump} should be a selector, use \"\\#{declaration}\" instead."
    end

    protected

    # Computes the CSS for the property.
    #
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String] The resulting CSS
    def _to_s(tabs)
      to_return = '  ' * (tabs - 1 + self.tabs) + name + ":" +
        (style == :compressed ? '' : ' ') + value + (style == :compressed ? "" : ";")
    end

    # Converts nested properties into flat properties.
    #
    # @param parent [PropNode, nil] The parent node of this node,
    #   or nil if the parent isn't a {PropNode}
    # @raise [Sass::SyntaxError] if the property uses invalid syntax
    def _cssize(parent)
      node = super
      result = node.children.dup
      if !node.value.empty? || node.children.empty?
        node.send(:check!)
        result.unshift(node)
      end
      result
    end

    # Updates the name and indentation of this node based on the parent name
    # and nesting level.
    #
    # @param parent [PropNode, nil] The parent node of this node,
    #   or nil if the parent isn't a {PropNode}
    def cssize!(parent)
      self.name = "#{parent.name}-#{name}" if parent
      self.tabs = parent.tabs + (parent.value.empty? ? 0 : 1) if parent && style == :nested
      super
    end

    # Runs any SassScript that may be embedded in the property,
    # and invludes the parent property, if any.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    def perform!(environment)
      @name = interpolate(@name, environment)
      @value = @value.is_a?(String) ? interpolate(@value, environment) : @value.perform(environment).to_s
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
      elsif value[-1] == ?;
        raise Sass::SyntaxError.new("Invalid property: #{declaration.dump} (no \";\" required at end-of-line).")
      elsif value.empty?
        raise Sass::SyntaxError.new("Invalid property: #{declaration.dump} (no value)." +
          pseudo_class_selector_message)
      end
    end

    def declaration
      (@prop_syntax == :new ? "#{name}: #{value}" : ":#{name} #{value}").strip
    end
  end
end
