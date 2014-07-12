module Sass::Tree
  # A static node representing a CSS property.
  #
  # @see Sass::Tree
  class PropNode < Node
    # The name of the property,
    # interspersed with {Sass::Script::Tree::Node}s
    # representing `#{}`-interpolation.
    # Any adjacent strings will be merged together.
    #
    # @return [Array<String, Sass::Script::Tree::Node>]
    attr_accessor :name

    # The name of the property
    # after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_name

    # The value of the property.
    #
    # @return [Sass::Script::Tree::Node]
    attr_accessor :value

    # The value of the property
    # after any interpolated SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
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

    # The source range in which the property name appears.
    #
    # @return [Sass::Source::Range]
    attr_accessor :name_source_range

    # The source range in which the property value appears.
    #
    # @return [Sass::Source::Range]
    attr_accessor :value_source_range

    # @param name [Array<String, Sass::Script::Tree::Node>] See \{#name}
    # @param value [Sass::Script::Tree::Node] See \{#value}
    # @param prop_syntax [Symbol] `:new` if this property uses `a: b`-style syntax,
    #   `:old` if it uses `:a b`-style syntax
    def initialize(name, value, prop_syntax)
      @name = Sass::Util.strip_string_array(
        Sass::Util.merge_adjacent_strings(name))
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
      if @prop_syntax == :new ||
          !value.is_a?(Sass::Script::Tree::Literal) ||
          !value.value.is_a?(Sass::Script::Value::String) ||
          !value.value.value.empty?
        return ""
      end

      "\nIf #{declaration.dump} should be a selector, use \"\\#{declaration}\" instead."
    end

    # Computes the Sass or SCSS code for the variable declaration.
    # This is like \{#to\_scss} or \{#to\_sass},
    # except it doesn't print any child properties or a trailing semicolon.
    #
    # @param opts [{Symbol => Object}] The options hash for the tree.
    # @param fmt [Symbol] `:scss` or `:sass`.
    def declaration(opts = {:old => @prop_syntax == :old}, fmt = :sass)
      name = self.name.map {|n| n.is_a?(String) ? n : n.to_sass(opts)}.join
      if name[0] == ?:
        raise Sass::SyntaxError.new("The \"#{name}: #{self.class.val_to_sass(value, opts)}\"" +
                                    " hack is not allowed in the Sass indented syntax")
      end

      old = opts[:old] && fmt == :sass
      initial = old ? ':' : ''
      mid = old ? '' : ':'
      "#{initial}#{name}#{mid} #{self.class.val_to_sass(value, opts)}".rstrip
    end

    # A property node is invisible if its value is empty.
    #
    # @return [Boolean]
    def invisible?
      resolved_value.empty?
    end

    private

    def check!
      if @options[:property_syntax] && @options[:property_syntax] != @prop_syntax
        raise Sass::SyntaxError.new(
          "Illegal property syntax: can't use #{@prop_syntax} syntax when " +
          ":property_syntax => #{@options[:property_syntax].inspect} is set.")
      end
    end

    class << self
      # @private
      def val_to_sass(value, opts)
        val_to_sass_comma(value, opts).to_sass(opts)
      end

      private

      def val_to_sass_comma(node, opts)
        return node unless node.is_a?(Sass::Script::Tree::Operation)
        return val_to_sass_concat(node, opts) unless node.operator == :comma

        Sass::Script::Tree::Operation.new(
          val_to_sass_concat(node.operand1, opts),
          val_to_sass_comma(node.operand2, opts),
          node.operator)
      end

      def val_to_sass_concat(node, opts)
        return node unless node.is_a?(Sass::Script::Tree::Operation)
        return val_to_sass_div(node, opts) unless node.operator == :space

        Sass::Script::Tree::Operation.new(
          val_to_sass_div(node.operand1, opts),
          val_to_sass_concat(node.operand2, opts),
          node.operator)
      end

      def val_to_sass_div(node, opts)
        unless node.is_a?(Sass::Script::Tree::Operation) && node.operator == :div &&
            node.operand1.is_a?(Sass::Script::Tree::Literal) &&
            node.operand1.value.is_a?(Sass::Script::Value::Number) &&
            node.operand2.is_a?(Sass::Script::Tree::Literal) &&
            node.operand2.value.is_a?(Sass::Script::Value::Number) &&
            (!node.operand1.value.original || !node.operand2.value.original)
          return node
        end

        Sass::Script::Value::String.new("(#{node.to_sass(opts)})")
      end
    end
  end
end
