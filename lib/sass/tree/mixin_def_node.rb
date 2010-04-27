module Sass
  module Tree
    # A dynamic node representing a mixin definition.
    #
    # @see Sass::Tree
    class MixinDefNode < Node
      # @param name [String] The mixin name
      # @param args [Array<(Script::Node, Script::Node)>] The arguments for the mixin.
      #   Each element is a tuple containing the variable for argument
      #   and the parse tree for the default value of the argument
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        args =
          if @args.empty?
            ""
          else
            '(' + @args.map do |v, d|
              if d
                "#{v.to_sass(opts)}: #{d.to_sass(opts)}"
              else
                v.to_sass(opts)
              end
            end.join(", ") + ')'
          end
              
        "#{'  ' * tabs}#{fmt == :sass ? '=' : '@mixin '}#{dasherize(@name, opts)}#{args}" +
          children_to_src(tabs, opts, fmt)
      end

      # Loads the mixin into the environment.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        environment.set_mixin(@name, Sass::Mixin.new(@name, @args, environment, children))
        []
      end

      # Returns an error message if the given child node is invalid,
      # and false otherwise.
      #
      # {ExtendNode}s are valid within {MixinDefNode}s.
      #
      # @param child [Tree::Node] A potential child node.
      # @return [Boolean, String] Whether or not the child node is valid,
      #   as well as the error message to display if it is invalid
      def invalid_child?(child)
        super unless child.is_a?(ExtendNode)
      end
    end
  end
end
