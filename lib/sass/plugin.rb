require 'sass'

module Sass
  # This module handles the compilation of Sass files.
  # It provides global options and checks whether CSS files
  # need to be updated.
  #
  # This module is used as the primary interface with Sass
  # when it's used as a plugin for various frameworks.
  # All Rack-enabled frameworks are supported out of the box.
  # The plugin is {file:SASS_REFERENCE.md#rails_merb_plugin automatically activated for Rails and Merb}.
  # Other frameworks must enable it explicitly; see {Sass::Plugin::Rack}.
  module Plugin
    extend self

    @options = {
      :css_location       => './public/stylesheets',
      :always_update      => false,
      :always_check       => true,
      :full_exception     => true
    }
    @checked_for_updates = false

    # Whether or not Sass has **ever** checked if the stylesheets need to be updated
    # (in this Ruby instance).
    #
    # @return [Boolean]
    attr_reader :checked_for_updates

    # An options hash.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # Sets the options hash.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @param value [{Symbol => Object}] The options hash
    def options=(value)
      @options.merge!(value)
    end

    # Non-destructively modifies \{#options} so that default values are properly set.
    #
    # @param additional_options [{Symbol => Object}] An options hash with which to merge \{#options}
    # @return [{Symbol => Object}] The modified options hash
    def engine_options(additional_options = {})
      opts = options.dup.merge(additional_options)
      opts[:load_paths] = load_paths(opts)
      opts
    end

    # Same as \{#update\_stylesheets}, but respects \{#checked\_for\_updates}
    # and the {file:SASS_REFERENCE.md#always_update-option `:always_update`}
    # and {file:SASS_REFERENCE.md#always_check-option `:always_check`} options.
    #
    # @see #update_stylesheets
    def check_for_updates
      return unless !Sass::Plugin.checked_for_updates ||
          Sass::Plugin.options[:always_update] || Sass::Plugin.options[:always_check]
      update_stylesheets
    end

    # Updates out-of-date stylesheets.
    #
    # Checks each Sass file in {file:SASS_REFERENCE.md#template_location-option `:template_location`}
    # to see if it's been modified more recently than the corresponding CSS file
    # in {file:SASS_REFERENCE.md#css_location-option} `:css_location`}.
    # If it has, it updates the CSS file.
    def update_stylesheets
      return if options[:never_update]

      @checked_for_updates = true
      template_locations.zip(css_locations).each do |template_location, css_location|

        Dir.glob(File.join(template_location, "**", "*.sass")).each do |file|
          # Get the relative path to the file with no extension
          name = file.sub(template_location.sub(/\/*$/, '/'), "")[0...-5]

          if !forbid_update?(name) && (options[:always_update] || stylesheet_needs_update?(name, template_location, css_location))
            update_stylesheet(name, template_location, css_location)
          end
        end
      end
    end

    private

    def update_stylesheet(name, template_location, css_location)
      css = css_filename(name, css_location)
      File.delete(css) if File.exists?(css)

      filename = template_filename(name, template_location)
      result = begin
                 Sass::Files.tree_for(filename, engine_options(:css_filename => css, :filename => filename)).render
               rescue Exception => e
                 raise e unless options[:full_exception]
                 exception_string(e)
               end

      # Create any directories that might be necessary
      mkpath(css_location, name)

      # Finally, write the file
      File.open(css, 'w') do |file|
        file.print(result)
      end
    end
    
    # Create any successive directories required to be able to write a file to: File.join(base,name)
    def mkpath(base, name)
      dirs = [base]
      name.split(File::SEPARATOR)[0...-1].each { |dir| dirs << File.join(dirs[-1],dir) }
      dirs.each { |dir| Dir.mkdir(dir) unless File.exist?(dir) }
    end

    def load_paths(opts = options)
      (opts[:load_paths] || []) + template_locations
    end
    
    def template_locations
      location = (options[:template_location] || File.join(options[:css_location],'sass'))
      if location.is_a?(String)
        [location]
      else
        location.to_a.map { |l| l.first }
      end
    end
    
    def css_locations
      if options[:template_location] && !options[:template_location].is_a?(String)
        options[:template_location].to_a.map { |l| l.last }
      else
        [options[:css_location]]
      end
    end

    def exception_string(e)
      e_string = "#{e.class}: #{e.message}"

      if e.is_a? Sass::SyntaxError
        e_string << "\non line #{e.sass_line}"

        if e.sass_filename
          e_string << " of #{e.sass_filename}"

          if File.exists?(e.sass_filename)
            e_string << "\n\n"

            min = [e.sass_line - 5, 0].max
            begin
              File.read(e.sass_filename).rstrip.split("\n")[
                min .. e.sass_line + 5
              ].each_with_index do |line, i|
                e_string << "#{min + i + 1}: #{line}\n"
              end
            rescue
              e_string << "Couldn't read sass file: #{e.sass_filename}"
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
    end

    def template_filename(name, path)
      "#{path}/#{name}.sass"
    end

    def css_filename(name, path)
      "#{path}/#{name}.css"
    end

    def forbid_update?(name)
      name.sub(/^.*\//, '')[0] == ?_
    end

    def stylesheet_needs_update?(name, template_path, css_path)
      css_file = css_filename(name, css_path)
      template_file = template_filename(name, template_path)
      exact_stylesheet_needs_update?(css_file, template_file)
    end

    def exact_stylesheet_needs_update?(css_file, template_file)
      return true unless File.exists?(css_file)

      css_mtime = File.mtime(css_file)
      File.mtime(template_file) > css_mtime ||
        dependencies(template_file).any?(&dependency_updated?(css_mtime))
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
          Sass::Files.find_file_to_import(inc.strip, [File.dirname(filename)] + load_paths)
        end
      end.flatten.grep(/\.sass$/)
    end
  end
end

require 'sass/plugin/rails' if defined?(ActionController)
require 'sass/plugin/merb'  if defined?(Merb::Plugins)
