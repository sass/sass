module Sass
  module Tree
    # A dynamic node representing a function definition.
    #
    # @see Sass::Tree
    class FunctionNode < Node
      def initialize(name, args)
        @name = name
        @args = args
        super()
      end

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        args = @args.map do |v, d|
          d ? "#{v.to_sass(opts)}: #{d.to_sass(opts)}" : v.to_sass(opts)
        end.join(", ")

        "#{'  ' * tabs}@function #{dasherize(@name, opts)}(#{args})" +
          children_to_src(tabs, opts, fmt)
      end

      # Loads the function into the environment.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable, mixin, and function values
      def _perform(environment)
        environment.set_function(@name, Sass::Callable.new(@name, @args, environment, children))
        []
      end

      # Returns an error message if the given child node is invalid,
      # and false otherwise.
      #
      # {FunctionNode}s may only contain variable declarations and SassScript directives
      # (e.g. `@if`, `@each`, `@warn`).
      # @param child [Tree::Node] A potential child node
      # @return [String] An error message if the child is invalid, or nil otherwise
      def invalid_child?(child)
        super ||
          unless [CommentNode, DebugNode, EachNode, ForNode,
              IfNode, VariableNode, WarnNode, WhileNode].any? {|c| child.is_a?(c)}
            "Functions can only contain variable declarations and certain directives."
          end
      end
    end
  end
end
