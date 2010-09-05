module Sass
  module Importers
    class FilesystemImporter < Base
      def initialize(root)
        @root = root
      end

      def find(name, context = nil)
        full_filename = nil
        full_filename = detect_within(File.dirname(context.filename), name) if context
        full_filename ||= detect_within(@root, name)
        return unless full_filename && File.readable?(full_filename)
        dirname, basename, extension = split(full_filename)
        SassFile.new(File.expand_path(full_filename), extension.to_sym, File.read(full_filename), self)
      end

      def detect_within(dir, name)
        each_possible_filename(name) do |possible|
          if File.exists?(full_path = "#{dir}/#{possible}")
            return full_path
          end
        end
        nil
      end

      def to_s
        @root
      end

      def on_disk?
        true
      end
    end
  end
end