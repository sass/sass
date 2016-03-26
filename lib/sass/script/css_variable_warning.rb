module Sass
  module Script
    # An object tracking whether a warning has been emitted for a given script
    # tree.
    #
    # This is shared among all objects in a script tree. Whenever any of those
    # objects encounters a situation in which it wouldn't produce semantically
    # identical CSS to its input, it calls \{#warn!\}. The first time \{#warn!}
    # is called for a given warning object, it prints a deprecation warning.
    class CssVariableWarning
      def initialize
        @warned = false
        @value = nil
      end

      # Sets the root of the script tree that this warning refers to.
      #
      # @param value [Sass::Script::Tree::Node]
      def value=(value)
        warn_called = @warned && !@value
        @value = value
        print_warning if warn_called
      end

      # The first time this is called, it prints a deprecation warning.
      #
      # This may be called before \{#value=}. If it is, the warning is emitted
      # once the script tree is set.
      def warn!
        return if @warned
        @warned = true
        return unless @value

        print_warning
      end

      private

      # Prints this node's warning.
      def print_warning
        of_filename = " of #{@value.filename}" if @value.filename
        Sass::Util.sass_warn(
          "DEPRECATION WARNING on line #{@value.line}#{of_filename}:\n" +
          "Sass 3.6 will change the way CSS variables are parsed. Instead of being parsed as\n" +
          "normal properties, they will not allow any Sass-specific behavior other than \#{}.\n" +
          "For forwards-compatibility, use \#{}:\n" +
          "\n" +
          "  --variable: \#{#{@value.to_sass}};")
      end
    end
  end
end
