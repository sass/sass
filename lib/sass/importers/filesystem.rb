require 'pathname'
require 'set'

module Sass
  module Importers
    # The default importer, used for any strings found in the load path.
    # Simply loads Sass files from the filesystem using the default logic.
    class Filesystem < Base

      attr_accessor :root

      # Creates a new filesystem importer that imports files relative to a given path.
      #
      # @param root [String] The root path.
      #   This importer will import files relative to this path.
      def initialize(root)
        @root = File.expand_path(root)
        @same_name_warnings = Set.new
      end

      # @see Base#find_relative
      def find_relative(name, base, options)
        _find(File.dirname(base), name, options)
      end

      # @see Base#find
      def find(name, options)
        _find(@root, name, options)
      end

      # @see Base#mtime
      def mtime(name, options)
        file, s = Sass::Util.destructure(find_real_file(@root, name, options))
        File.mtime(file) if file
      rescue Errno::ENOENT
        nil
      end

      # @see Base#key
      def key(name, options)
        [self.class.name + ":" + File.dirname(File.expand_path(name)),
          File.basename(name)]
      end

      # @see Base#to_s
      def to_s
        @root
      end

      def hash
        @root.hash
      end

      def eql?(other)
        root.eql?(other.root)
      end

      protected

      # If a full uri is passed, this removes the root from it
      # otherwise returns the name unchanged
      def remove_root(name)
        if name.index(@root + "/") == 0
          name[(@root.length + 1)..-1]
        else
          name
        end
      end

      # A hash from file extensions to the syntaxes for those extensions.
      # The syntaxes must be `:sass` or `:scss`.
      #
      # This can be overridden by subclasses that want normal filesystem importing
      # with unusual extensions.
      #
      # @return [{String => Symbol}]
      def extensions
        {'sass' => :sass, 'scss' => :scss}
      end

      # Given an `@import`ed path, returns an array of possible
      # on-disk filenames and their corresponding syntaxes for that path.
      #
      # @param name [String] The filename.
      # @return [Array(String, Symbol)] An array of pairs.
      #   The first element of each pair is a filename to look for;
      #   the second element is the syntax that file would be in (`:sass` or `:scss`).
      def possible_files(name)
        name = escape_glob_characters(name)
        dirname, basename, extname = split(name)
        sorted_exts = extensions.sort
        syntax = extensions[extname]

        if syntax
          ret = [["#{dirname}/{_,}#{basename}.#{extensions.invert[syntax]}", syntax]]
        else
          ret = sorted_exts.map {|ext, syn| ["#{dirname}/{_,}#{basename}.#{ext}", syn]}
        end

        # JRuby chokes when trying to import files from JARs when the path starts with './'.
        ret.map {|f, s| [f.sub(%r{^\./}, ''), s]}
      end

      def escape_glob_characters(name)
        name.gsub(/[\*\[\]\{\}\?]/) do |char|
          "\\#{char}"
        end
      end

      REDUNDANT_DIRECTORY = %r{#{Regexp.escape(File::SEPARATOR)}\.#{Regexp.escape(File::SEPARATOR)}}
      # Given a base directory and an `@import`ed name,
      # finds an existant file that matches the name.
      #
      # @param dir [String] The directory relative to which to search.
      # @param name [String] The filename to search for.
      # @return [(String, Symbol)] A filename-syntax pair.
      def find_real_file(dir, name, options)
        # on windows 'dir' can be in native File::ALT_SEPARATOR form
        dir = dir.gsub(File::ALT_SEPARATOR, File::SEPARATOR) unless File::ALT_SEPARATOR.nil?

        found = possible_files(remove_root(name)).map do |f, s|
          path = (dir == "." || Pathname.new(f).absolute?) ? f : "#{dir}/#{f}"
          Dir[path].map do |full_path|
            full_path.gsub!(REDUNDANT_DIRECTORY, File::SEPARATOR)
            [full_path, s]
          end
        end
        found = Sass::Util.flatten(found, 1)
        return if found.empty?

        if found.size > 1 && !@same_name_warnings.include?(found.first.first)
          found.each {|(f, _)| @same_name_warnings << f}
          relative_to = Pathname.new(dir)
          if options[:_line]
            # If _line exists, we're here due to an actual import in an
            # import_node and we want to print a warning for a user writing an
            # ambiguous import.
            candidates = found.map {|(f, _)| "    " + Pathname.new(f).relative_path_from(relative_to).to_s}.join("\n")
            Sass::Util.sass_warn <<WARNING
WARNING: On line #{options[:_line]}#{" of #{options[:filename]}" if options[:filename]}:
  It's not clear which file to import for '@import "#{name}"'.
  Candidates:
#{candidates}
  For now I'll choose #{File.basename found.first.first}.
  This will be an error in future versions of Sass.
WARNING
          else
            # Otherwise, we're here via StalenessChecker, and we want to print a
            # warning for a user running `sass --watch` with two ambiguous files.
            candidates = found.map {|(f, _)| "    " + File.basename(f)}.join("\n")
            Sass::Util.sass_warn <<WARNING
WARNING: In #{File.dirname(name)}:
  There are multiple files that match the name "#{File.basename(name)}":
#{candidates}
WARNING
          end
        end
        found.first
      end

      # Splits a filename into three parts, a directory part, a basename, and an extension
      # Only the known extensions returned from the extensions method will be recognized as such.
      def split(name)
        extension = nil
        dirname, basename = File.dirname(name), File.basename(name)
        if basename =~ /^(.*)\.(#{extensions.keys.map{|e| Regexp.escape(e)}.join('|')})$/
          basename = $1
          extension = $2
        end
        [dirname, basename, extension]
      end

      private

      def _find(dir, name, options)
        full_filename, syntax = Sass::Util.destructure(find_real_file(dir, name, options))
        return unless full_filename && File.readable?(full_filename)

        options[:syntax] = syntax
        options[:filename] = full_filename
        options[:importer] = self
        Sass::Engine.new(File.read(full_filename), options)
      end

      def join(base, path)
        Pathname.new(base).join(path).to_s
      end
    end
  end
end
