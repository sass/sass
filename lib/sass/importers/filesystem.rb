module Sass
  module Importers
    class Filesystem < Base
      def initialize(root)
        @root = root
      end

      def find_relative(name, base, options)
        _find(detect_within(File.dirname(base), name), options)
      end

      def find(name, options)
        _find(detect_within(@root, name), options)
      end

      def to_s
        @root
      end

      def on_disk?
        true
      end

      private

      def _find(full_filename, options)
        return unless full_filename && File.readable?(full_filename)

        options[:syntax] = File.extname(full_filename)[1..-1].to_sym
        return unless [:sass, :scss, :css].include?(options[:syntax])

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
