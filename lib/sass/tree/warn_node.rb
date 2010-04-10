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

      def to_src(tabs, opts, fmt)
        "#{'  ' * tabs}@warn #{@expr.to_sass(opts)}#{semi fmt}\n"
      end

      # Prints the expression to STDERR with a stylesheet trace.
      #
      # @param environment [Sass::Environment] The lexical environment containing
      #   variable and mixin values
      def _perform(environment)
        environment.push(:filename => filename, :line => line)
        res = @expr.perform(environment)
        res = res.value if res.is_a?(Sass::Script::String)
        Haml::Util.haml_warn "WARNING: #{res}"
        Haml::Util.enum_with_index(environment.stack.reverse).each do |entry, i|
          where = "         "
          if entry[:mixin]
            where << "via '#{entry[:mixin]}' mixed in at line #{entry[:line]}"
          elsif entry[:import]
            where << "imported from line #{entry[:line]}"
          else
            where << "#{"issued" if i == 0} from line #{entry[:line]}"
          end
          where << " of #{entry[:filename] || "(sass)"}"
          Haml::Util.haml_warn where
        end
        []
      ensure
        environment.pop
      end
    end
  end
end
