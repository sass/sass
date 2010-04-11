module Sass
  module Plugin
    class StalenessChecker
      @dependencies_cache = {}

      class << self
        attr_accessor :dependencies_cache
      end
      
      def initialize(dependencies = self.class.dependencies_cache)
        @dependencies                = dependencies
        @mtimes, @dependencies_stale = {}, {}
      end

      def stylesheet_needs_update?(css_file, template_file)
        template_file = File.expand_path(template_file)

        unless File.exists?(css_file) && File.exists?(template_file)
          @dependencies.delete(template_file)
          true
        else
          css_mtime = mtime(css_file)
          mtime(template_file) > css_mtime || dependencies_stale?(template_file, css_mtime)
        end
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
        @mtimes[filename] ||= File.mtime(filename)
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