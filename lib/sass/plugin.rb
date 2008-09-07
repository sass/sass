require 'sass/engine'

module Sass
  # This module contains methods to aid in using Sass
  # as a stylesheet-rendering plugin for various systems.
  # Currently Rails/ActionController and Merb are supported out of the box.
  module Plugin
    class << self
      @@options = {
        :template_location  => './public/stylesheets/sass',
        :css_location       => './public/stylesheets',
        :always_update      => false,
        :always_check       => true,
        :full_exception     => true
      }
      @@checked_for_updates = false

      # Whether or not Sass has *ever* checked if the stylesheets need updates
      # (in this Ruby instance).
      def checked_for_updates
        @@checked_for_updates
      end

      # Gets various options for Sass. See README.rdoc for details.
      #--
      # TODO: *DOCUMENT OPTIONS*
      #++
      def options
        @@options
      end

      # Sets various options for Sass.
      def options=(value)
        @@options.merge!(value)
      end

      # Get the options ready to be passed to the Sass::Engine
      def engine_options(additional_options = {})
        opts = options.dup.merge(additional_options)
        opts[:load_paths] = load_paths(opts)
        opts
      end

      # Checks each stylesheet in <tt>options[:css_location]</tt>
      # to see if it needs updating,
      # and updates it using the corresponding template
      # from <tt>options[:templates]</tt>
      # if it does.
      def update_stylesheets
        return if options[:never_update]

        @@checked_for_updates = true
        Dir.glob(File.join(options[:template_location], "**", "*.sass")).entries.each do |file|

          # Get the relative path to the file with no extension
          name = file.sub(options[:template_location] + "/", "")[0...-5]

          if !forbid_update?(name) && (options[:always_update] || stylesheet_needs_update?(name))
            css = css_filename(name)
            File.delete(css) if File.exists?(css)

            filename = template_filename(name)
            engine = Engine.new(File.read(filename), engine_options(:filename => filename))
            result = begin
                       engine.render
                     rescue Exception => e
                       exception_string(e)
                     end

            # Create any directories that might be necessary
            dirs = [options[:css_location]]
            name.split("/")[0...-1].each { |dir| dirs << "#{dirs[-1]}/#{dir}" }
            dirs.each { |dir| Dir.mkdir(dir) unless File.exist?(dir) }

            # Finally, write the file
            File.open(css, 'w') do |file|
              file.print(result)
            end
          end
        end
      end

      private

      def load_paths(opts = options)
        (opts[:load_paths] || []) + [options[:template_location]]
      end

      def exception_string(e)
        if options[:full_exception]
          e_string = "#{e.class}: #{e.message}"

          if e.is_a? Sass::SyntaxError
            e_string << "\non line #{e.sass_line}"

            if e.sass_filename
              e_string << " of #{e.sass_filename}"

              if File.exists?(e.sass_filename)
                e_string << "\n\n"

                min = [e.sass_line - 5, 0].max
                File.read(e.sass_filename).rstrip.split("\n")[
                  min .. e.sass_line + 5
                ].each_with_index do |line, i|
                  e_string << "#{min + i + 1}: #{line}\n"
                end
              end
            end
          end
          <<END
/*
#{e_string}

Backtrace:\n#{e.backtrace.join("\n")}
*/
body:before {
  white-space: pre;
  font-family: monospace;
  content: "#{e_string.gsub('"', '\"').gsub("\n", '\\A ')}"; }
END
          # Fix an emacs syntax-highlighting hiccup: '
        else
          "/* Internal stylesheet error */"
        end
      end

      def template_filename(name)
        "#{options[:template_location]}/#{name}.sass"
      end

      def css_filename(name)
        "#{options[:css_location]}/#{name}.css"
      end

      def forbid_update?(name)
        name.sub(/^.*\//, '')[0] == ?_
      end

      def stylesheet_needs_update?(name)
        if !File.exists?(css_filename(name))
          return true
        else
          css_mtime = File.mtime(css_filename(name))
          File.mtime(template_filename(name)) > css_mtime ||
            dependencies(template_filename(name)).any?(&dependency_updated?(css_mtime))
        end
      end

      def dependency_updated?(css_mtime)
        lambda do |dep|
          File.mtime(dep) > css_mtime ||
            dependencies(dep).any?(&dependency_updated?(css_mtime))
        end
      end

      def dependencies(filename)
        File.readlines(filename).grep(/^@import /).map do |line|
          line[8..-1].split(',').map do |inc|
            Sass::Engine.find_file_to_import(inc.strip, load_paths)
          end
        end.flatten.grep(/\.sass$/)
      end
    end
  end
end

require 'sass/plugin/rails' if defined?(ActionController)
require 'sass/plugin/merb'  if defined?(Merb::Plugins)
