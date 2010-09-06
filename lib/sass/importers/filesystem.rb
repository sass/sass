module Sass
  module Importers
    # The default importer, used for any strings found in the load path.
    # Simply loads Sass files from the filesystem using the default logic.
    class Filesystem < Base
      # Creates a new filesystem importer that imports files relative to a given path.
      #
      # @param root [String] The root path.
      #   This importer will import files relative to this path.
      def initialize(root)
        @root = root
      end

      # @see Base#find_relative
      def find_relative(name, base, options)
        _find(detect_within(File.dirname(base), name), options)
      end

      # @see Base#find
      def find(name, options)
        _find(detect_within(@root, name), options)
      end

      # @see Base#to_s
      def to_s
        @root
      end

      # @see Base#on_disk?
      def on_disk?
        true
      end

      private

      def _find(full_filename, options)
        return unless full_filename && File.readable?(full_filename)

        dirname, basename, extension = split(full_filename)
        options[:syntax] = determine_syntax_from_extension(extension)
        options[:filename] = full_filename
        options[:importer] = self
        Sass::Engine.new(File.read(full_filename), options)
      end

      def detect_within(dir, name)
        each_possible_filename(name) do |possible|
          if File.exists?(full_path = "#{dir}/#{possible}")
            return full_path
          end
        end
        nil
      end
    end
  end
end
