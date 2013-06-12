module Sass
  module Importers
    # This importer emits a deprecation warning the first time it is used to
    # import a file. It is used to deprecate the current working
    # directory from the list of automatic sass load paths.
    class DeprecatedPath < Filesystem

      def initialize(root)
        @specified_root = root
        @warning_given = false
        super
      end

      def find(*args)
        found = super
        if found && !@warning_given
          @warning_given = true
          Sass::Util.sass_warn deprecation_warning
        end
        found
      end

      def deprecation_warning
        path = (@specified_root == ".") ? "the current working directory" : @specified_root
        <<WARNING
DEPRECATION WARNING: Importing from #{path} will not be
automatic in future versions of Sass.  To avoid future errors, you can add it
to your environment explicitly by setting `SASSPATH=#{@specified_root}`, by using the -I command
line option, or by changing your Sass configuration options.
WARNING
      end

      def to_s
        "#{@root} (DEPRECATED)"
      end
    end
  end
end
