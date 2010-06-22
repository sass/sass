require 'fileutils'
require 'rbconfig'

require 'sass'
require 'sass/plugin/configuration'
require 'sass/plugin/staleness_checker'

module Sass
  # This module handles the compilation of Sass/SCSS files.
  # It provides global options and checks whether CSS files
  # need to be updated.
  #
  # This module is used as the primary interface with Sass
  # when it's used as a plugin for various frameworks.
  # All Rack-enabled frameworks are supported out of the box.
  # The plugin is {file:SASS_REFERENCE.md#rails_merb_plugin automatically activated for Rails and Merb}.
  # Other frameworks must enable it explicitly; see {Sass::Plugin::Rack}.
  #
  # This module has a large set of callbacks available
  # to allow users to run code (such as logging) when certain things happen.
  # All callback methods are of the form `on_#{name}`,
  # and they all take a block that's called when the given action occurs.
  #
  # @example Using a callback
  # Sass::Plugin.on_updating_stylesheet do |template, css|
  #   puts "Compiling #{template} to #{css}"
  # end
  # Sass::Plugin.update_stylesheets
  #   #=> Compiling app/sass/screen.scss to public/stylesheets/screen.css
  #   #=> Compiling app/sass/print.scss to public/stylesheets/print.css
  #   #=> Compiling app/sass/ie.scss to public/stylesheets/ie.css
  module Plugin
    include Haml::Util

    @checked_for_updates = false

    # Whether or not Sass has **ever** checked if the stylesheets need to be updated
    # (in this Ruby instance).
    #
    # @return [Boolean]
    attr_reader :checked_for_updates

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
    # Checks each Sass/SCSS file in {file:SASS_REFERENCE.md#template_location-option `:template_location`}
    # to see if it's been modified more recently than the corresponding CSS file
    # in {file:SASS_REFERENCE.md#css_location-option `:css_location`}.
    # If it has, it updates the CSS file.
    #
    # @param individual_files [Array<(String, String)>]
    #   A list of files to check for updates
    #   **in addition to those specified by the
    #   {file:SASS_REFERENCE.md#template_location-option `:template_location` option}.**
    #   The first string in each pair is the location of the Sass/SCSS file,
    #   the second is the location of the CSS file that it should be compiled to.
    def update_stylesheets(individual_files = [])
      return if options[:never_update]

      run_updating_stylesheets individual_files

      individual_files.each {|t, c| update_stylesheet(t, c)}

      @checked_for_updates = true
      staleness_checker = StalenessChecker.new

      template_location_array.each do |template_location, css_location|

        Dir.glob(File.join(template_location, "**", "*.s[ca]ss")).each do |file|
          # Get the relative path to the file
          name = file.sub(template_location.sub(/\/*$/, '/'), "")
          css = css_filename(name, css_location)

          next if forbid_update?(name)
          if options[:always_update] || staleness_checker.stylesheet_needs_update?(css, file)
            update_stylesheet file, css
          else
            run_not_updating_stylesheet file, css
          end
        end
      end
    end

    # Updates all stylesheets, even those that aren't out-of-date.
    # Ignores the cache.
    #
    # @param individual_files [Array<(String, String)>]
    #   A list of files to check for updates
    #   **in addition to those specified by the
    #   {file:SASS_REFERENCE.md#template_location-option `:template_location` option}.**
    #   The first string in each pair is the location of the Sass/SCSS file,
    #   the second is the location of the CSS file that it should be compiled to.
    # @see #update_stylesheets
    def force_update_stylesheets(individual_files = [])
      old_options = options
      self.options = options.dup
      options[:never_update] = false
      options[:always_update] = true
      options[:cache] = false
      update_stylesheets(individual_files)
    ensure
      self.options = old_options
    end

    # Watches the template directory (or directories)
    # and updates the CSS files whenever the related Sass/SCSS files change.
    # `watch` never returns.
    #
    # Whenever a change is detected to a Sass/SCSS file in
    # {file:SASS_REFERENCE.md#template_location-option `:template_location`},
    # the corresponding CSS file in {file:SASS_REFERENCE.md#css_location-option `:css_location`}
    # will be recompiled.
    # The CSS files of any Sass/SCSS files that import the changed file will also be recompiled.
    #
    # Before the watching starts in earnest, `watch` calls \{#update\_stylesheets}.
    #
    # Note that `watch` uses the [FSSM](http://github.com/ttilley/fssm) library
    # to monitor the filesystem for changes.
    # FSSM isn't loaded until `watch` is run.
    # The version of FSSM distributed with Sass is loaded by default,
    # but if another version has already been loaded that will be used instead.
    #
    # @param individual_files [Array<(String, String)>]
    #   A list of files to watch for updates
    #   **in addition to those specified by the
    #   {file:SASS_REFERENCE.md#template_location-option `:template_location` option}.**
    #   The first string in each pair is the location of the Sass/SCSS file,
    #   the second is the location of the CSS file that it should be compiled to.
    def watch(individual_files = [])
      update_stylesheets(individual_files)

      begin
        require 'fssm'
      rescue LoadError => e
        e.message << "\n" <<
          if File.exists?(scope(".git"))
            'Run "git submodule update --init" to get the recommended version.'
          else
            'Run "gem install fssm" to get it.'
          end
        raise e
      end

      unless individual_files.empty? && FSSM::Backends::Default.name == "FSSM::Backends::FSEvents"
        # As of FSSM 0.1.4, it doesn't support FSevents on individual files,
        # but it also isn't smart enough to switch to polling itself.
        require 'fssm/backends/polling'
        Haml::Util.silence_warnings do
          FSSM::Backends.const_set(:Default, FSSM::Backends::Polling)
        end
      end

      # TODO: Keep better track of what depends on what
      # so we don't have to run a global update every time anything changes.
      FSSM.monitor do |mon|
        template_location_array.each do |template_location, css_location|
          mon.path template_location do |path|
            path.glob '**/*.s[ac]ss'

            path.update do |base, relative|
              run_template_modified File.join(base, relative)
              update_stylesheets(individual_files)
            end

            path.create do |base, relative|
              run_template_created File.join(base, relative)
              update_stylesheets(individual_files)
            end

            path.delete do |base, relative|
              run_template_deleted File.join(base, relative)
              css = File.join(css_location, relative.gsub(/\.s[ac]ss$/, '.css'))
              try_delete_css css
              update_stylesheets(individual_files)
            end
          end
        end

        individual_files.each do |template, css|
          mon.file template do |path|
            path.update do
              run_template_modified template
              update_stylesheets(individual_files)
            end

            path.create do
              run_template_created template
              update_stylesheets(individual_files)
            end

            path.delete do
              run_template_deleted template
              try_delete_css css
              update_stylesheets(individual_files)
            end
          end
        end
      end
    end

    private

    def update_stylesheet(filename, css)
      dir = File.dirname(css)
      unless File.exists?(dir)
        run_creating_directory dir
        FileUtils.mkdir_p dir
      end

      begin
        result = Sass::Files.tree_for(filename, engine_options(:css_filename => css, :filename => filename)).render
      rescue Exception => e
        run_compilation_error e, filename, css
        result = Sass::SyntaxError.exception_to_css(e, options)
      else
        run_updating_stylesheet filename, css
      end

      # Finally, write the file
      flag = 'w'
      flag = 'wb' if RbConfig::CONFIG['host_os'] =~ /mswin|windows/i && options[:unix_newlines]
      File.open(css, flag) {|file| file.print(result)}
    end

    def try_delete_css(css)
      return unless File.exists?(css)
      run_deleting_css css
      File.delete css
    end

    def load_paths(opts = options)
      (opts[:load_paths] || []) + template_locations
    end

    def template_locations
      template_location_array.to_a.map {|l| l.first}
    end

    def css_locations
      template_location_array.to_a.map {|l| l.last}
    end

    def css_filename(name, path)
      "#{path}/#{name}".gsub(/\.s[ac]ss$/, '.css')
    end

    def forbid_update?(name)
      name.sub(/^.*\//, '')[0] == ?_
    end

    # Compass expects this to exist
    def stylesheet_needs_update?(css_file, template_file)
      StalenessChecker.stylesheet_needs_update?(css_file, template_file)
    end
  end
end

if defined?(ActionController)
  require 'sass/plugin/rails'
elsif defined?(Merb::Plugins)
  require 'sass/plugin/merb'
else
  require 'sass/plugin/generic'
end
