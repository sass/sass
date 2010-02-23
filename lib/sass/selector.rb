module Sass
  # A namespace for nodes in the parse tree for selectors.
  #
  # This parse tree is distinct from the normal Sass/SCSS parse tree.
  # It's used primarily for 
  module Selector
    class Node
      def to_a
        raise NotImplementedError.new("All static-node subclasses of Sass::Selector::Node must override #to_a.")
      end
    end

    class Parent < Node
      def to_a
        ["&"]
      end
    end

    class Class < Node
      def initialize(name)
        @name = name
      end

      def to_a
        [".", @name]
      end
    end

    class Id < Node
      def initialize(name)
        @name = name
      end

      def to_a
        ["#", @name]
      end
    end

    class Universal < Node
      def initialize(namespace)
        @namespace = namespace
      end

      def to_a
        @namespace ? [@namespace, "|*"] : ["*"]
      end
    end

    class Element < Node
      def initialize(name, namespace)
        @name = name
        @namespace = namespace
      end

      def to_a
        @namespace ? [@namespace, "|", @name] : [@name]
      end
    end

    class Interpolation < Node
      def initialize(script)
        @script = script
      end

      def to_a
        [@script]
      end
    end

    class Attribute < Node
      def initialize(name, namespace, operator, value)
        @name = name
        @namespace = namespace
        @operator = operator
        @value = value
      end

      def to_a
        res = ["["]
        res << @namespace << "|" if @namespace
        res << @name
        res << @operator << @value if @value
        res << "]"
      end
    end

    class Pseudo < Node
      def initialize(type, name, arg)
        @type = type
        @name = name
        @arg = arg
      end

      def to_a
        res = [@type == :class ? ":" : "::", @name]
        res << "(" << @arg << ")" if @arg
        res
      end
    end

    class Negation < Node
      def initialize(selector)
        @selector = selector
      end

      def to_a
        [":not("] + @selector.to_a + [")"]
      end
    end
  end
end
