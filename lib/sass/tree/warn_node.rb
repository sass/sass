module Sass
  module Tree
    # A dynamic node representing a Sass `@warn` statement.
    #
    # @see Sass::Tree
    class WarnNode < Node
      # @param expr [Script::Node] The expression to print
      def initialize(expr)
        @expr = expr
        super()
      end

      protected

      # @see Node#to_src
      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}@warn #{@expr.to_sass(opts)}#{semi fmt}\n"
      end

      # Prints the expression to STDERR with a stylesheet trace.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        environment.push_frame(:filename => filename, :line => line)
        res = @expr.perform(environment)
        res = res.value if res.is_a?(Sass::Script::String)
        msg = "WARNING: #{res}\n"
        environment.stack.reverse.each_with_index do |entry, i|
          msg << "        #{i == 0 ? "on" : "from"} line #{entry[:line]}" <<
            " of #{entry[:filename] || "an unknown file"}"
          msg << ", in `#{entry[:mixin]}'" if entry[:mixin]
          msg << "\n"
        end
        Haml::Util.haml_warn msg
        []
      ensure
        environment.pop_frame
      end
    end
  end
end
