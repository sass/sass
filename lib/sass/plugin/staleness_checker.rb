module Sass
  module Plugin
    # The class handles `.s[ca]ss` file staleness checks via their mtime timestamps.
    #
    # To speed things up two level of caches are employed:
    #
    # * A class-level dependency cache which stores @import paths for each file.
    #   This is a long-lived cache that is reused by every StalenessChecker instance.
    # * Two short-lived instance-level caches, one for file mtimes
    #   and one for whether a file is stale during this particular run.
    #   These are only used by a single StalenessChecker instance.
    #
    # Usage:
    #
    # * For a one-off staleness check of a single `.s[ca]ss` file,
    #   the class-level {stylesheet_needs_update?} method
    #   should be used.
    # * For a series of staleness checks (e.g. checking all files for staleness)
    #   a StalenessChecker instance should be created,
    #   and the instance-level \{#stylesheet\_needs\_update?} method should be used.
    #   the caches should make the whole process significantly faster.
    #   *WARNING*: It is important not to retain the instance for too long,
    #   as its instance-level caches are never explicitly expired.
    class StalenessChecker
      DELETED             = 1.0/0.0 # positive Infinity
      @dependencies_cache = {}

      class << self
        # @private
        attr_accessor :dependencies_cache
      end

      # Creates a new StalenessChecker
      # for checking the staleness of several stylesheets at once.
      def initialize
        @dependencies = self.class.dependencies_cache

        # Entries in the following instance-level caches are never explicitly expired.
        # Instead they are supposed to automaticaly go out of scope when a series of staleness checks
        # (this instance of StalenessChecker was created for) is finished.
        @mtimes, @dependencies_stale = {}, {}
      end

      # Returns whether or not a given CSS file is out of date
      # and needs to be regenerated.
      #
      # @param css_file [String] The location of the CSS file to check.
      # @param template_file [String] The location of the Sass or SCSS template
      #   that is compiled to `css_file`.
      def stylesheet_needs_update?(css_file, template_file)
        template_file, css_mtime = File.expand_path(template_file), mtime(css_file)

        css_mtime == DELETED || dependency_updated?(css_mtime).call(template_file)
      end

      # Returns whether or not a given CSS file is out of date
      # and needs to be regenerated.
      #
      # The distinction between this method and the instance-level \{#stylesheet\_needs\_update?}
      # is that the instance method preserves mtime and stale-dependency caches,
      # so it's better to use when checking multiple stylesheets at once.
      #
      # @param css_file [String] The location of the CSS file to check.
      # @param template_file [String] The location of the Sass or SCSS template
      #   that is compiled to `css_file`.
      def self.stylesheet_needs_update?(css_file, template_file)
        new.stylesheet_needs_update?(css_file, template_file)
      end

      private

      def dependencies_stale?(template_file, css_mtime)
        timestamps = @dependencies_stale[template_file] ||= {}
        timestamps.each_pair do |checked_css_mtime, is_stale|
          if checked_css_mtime <= css_mtime && !is_stale
            return false
          elsif checked_css_mtime > css_mtime && is_stale
            return true
          end
        end
        timestamps[css_mtime] = dependencies(template_file).any?(&dependency_updated?(css_mtime))
      end

      def mtime(filename)
        @mtimes[filename] ||= begin
          File.mtime(filename).to_i
        rescue Errno::ENOENT
          @dependencies.delete(filename)
          DELETED
        end
      end

      def dependencies(filename)
        stored_mtime, dependencies = @dependencies[filename]

        if !stored_mtime || stored_mtime < mtime(filename)
          @dependencies[filename] = [mtime(filename), dependencies = compute_dependencies(filename)]
        end

        dependencies
      end

      def dependency_updated?(css_mtime)
        lambda do |dep|
          begin
            mtime(dep) > css_mtime || dependencies_stale?(dep, css_mtime)
          rescue Sass::SyntaxError
            # If there's an error finding depenencies, default to recompiling.
            true
          end
        end
      end

      def compute_dependencies(filename)
        Files.tree_for(filename, Plugin.engine_options).grep(Tree::ImportNode) do |n|
          File.expand_path(n.full_filename) unless n.full_filename =~ /\.css$/
        end.compact
      rescue Sass::SyntaxError => e
        [] # If the file has an error, we assume it has no dependencies
      end
    end
  end
end
