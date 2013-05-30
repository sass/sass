module Sass
  module Importers
    # This importer emits a deprecation warning the first time it is used to
    # import a non-relative file. It is used to deprecate the current working
    # directory from the list of automatic sass load paths.
    class DeprecatedPath < Filesystem
      attr_reader :specified_root
      def initialize(root)
        @specified_root = root
        super
        @warning_given = false
      end
      def find(*args)
        found = super
        if found && !@warning_given
          @warning_given = true
          path = (specified_root == ".") ? "the current working directory" : root
          Sass::Util.sass_warn %Q{WARNING: Importing Sass files from #{path} is deprecated.
         If you need it, you can add it to your environment explicitly with
         SASSPATH=#{root} or by changing your Sass configuration options.}
        end
        found
      end
      def to_s
        "#{@root} (DEPRECATED)"
      end
    end
  end
end
