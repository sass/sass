require 'fileutils'
require 'rbconfig'

require 'sass'
require 'sass/callbacks'

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
  #   #=> Compiling app/sass/screen.sass to public/stylesheets/screen.css
  #   #=> Compiling app/sass/print.sass to public/stylesheets/print.css
  #   #=> Compiling app/sass/ie.sass to public/stylesheets/ie.css
  module Plugin
    include Haml::Util
    include Sass::Callbacks
    extend self

    @options = {
      :css_location       => './public/stylesheets',
      :always_update      => false,
      :always_check       => true,
      :full_exception     => true
    }
    @checked_for_updates = false

    # Register a callback to be run before stylesheets are mass-updated.
    # This is run whenever \{#update\_stylesheets} is called,
    # unless the \{file:SASS_REFERENCE.md#never_update-option `:never_update` option}
    # is enabled.
    #
    # @yield [individual_files]
    # @yieldparam individual_files [<(String, String)>]
    #   Individual files to be updated, in addition to the directories
    #   specified in the options.
    #   The first element of each pair is the source file,
    #   the second is the target CSS file.
    define_callback :updating_stylesheets

    # Register a callback to be run before a single stylesheet is updated.
    # The callback is only run if the stylesheet is guaranteed to be updated;
    # if the CSS file is fresh, this won't be run.
    #
    # Even if the \{file:SASS_REFERENCE.md#full_exception-option `:full_exception` option}
    # is enabled, this callback won't be run
    # when an exception CSS file is being written.
    # To run an action for those files, use \{#on\_compilation\_error}.
    #
    # @yield [template, css]
    # @yieldparam template [String]
    #   The location of the Sass file being updated.
    # @yieldparam css [String]
    #   The location of the CSS file being generated.
    define_callback :updating_stylesheet

    # Register a callback to be run when Sass decides not to update a stylesheet.
    # In particular, the callback is run when Sass finds that
    # the template file and none of its dependencies
    # have been modified since the last compilation.
    #
    # Note that this is **not** run when the
    # \{file:SASS_REFERENCE.md#never-update_option `:never_update` option} is set,
    # nor when Sass decides not to compile a partial.
    #
    # @yield [template, css]
    # @yieldparam template [String]
    #   The location of the Sass file not being updated.
    # @yieldparam css [String]
    #   The location of the CSS file not being generated.
    define_callback :not_updating_stylesheet

    # Register a callback to be run when there's an error
    # compiling a Sass file.
    # This could include not only errors in the Sass document,
    # but also errors accessing the file at all.
    #
    # @yield [error, template, css]
    # @yieldparam error [Exception] The exception that was raised.
    # @yieldparam template [String]
    #   The location of the Sass file being updated.
    # @yieldparam css [String]
    #   The location of the CSS file being generated.
    define_callback :compilation_error

    # Register a callback to be run when Sass creates a directory
    # into which to put CSS files.
    #
    # Note that even if multiple levels of directories need to be created,
    # the callback may only be run once.
    # For example, if "foo/" exists and "foo/bar/baz/" needs to be created,
    # this may only be run for "foo/bar/baz/".
    # This is not a guarantee, however;
    # it may also be run for "foo/bar/".
    #
    # @yield [dirname]
    # @yieldparam dirname [String]
    #   The location of the directory that was created.
    define_callback :creating_directory

    # Register a callback to be run when Sass detects
    # that a template has been modified.
    # This is only run when using \{#watch}.
    #
    # @yield [template]
    # @yieldparam template [String]
    #   The location of the template that was modified.
    define_callback :template_modified

    # Register a callback to be run when Sass detects
    # that a new template has been created.
    # This is only run when using \{#watch}.
    #
    # @yield [template]
    # @yieldparam template [String]
    #   The location of the template that was created.
    define_callback :template_created

    # Register a callback to be run when Sass detects
    # that a template has been deleted.
    # This is only run when using \{#watch}.
    #
    # @yield [template]
    # @yieldparam template [String]
    #   The location of the template that was deleted.
    define_callback :template_deleted

    # Register a callback to be run when Sass deletes a CSS file.
    # This happens when the corresponding Sass file has been deleted.
    #
    # @yield [filename]
    # @yieldparam filename [String]
    #   The location of the CSS file that was deleted.
    define_callback :deleting_css

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
    # in {file:SASS_REFERENCE.md#css_location-option `:css_location`}.
    # If it has, it updates the CSS file.
    #
    # @param individual_files [Array<(String, String)>]
    #   A list of files to check for updates
    #   **in addition to those specified by the
    #   {file:SASS_REFERENCE.md#template_location-option `:template_location` option}.**
    #   The first string in each pair is the location of the Sass file,
    #   the second is the location of the CSS file that it should be compiled to.
    def update_stylesheets(individual_files = [])
      return if options[:never_update]

      run_updating_stylesheets individual_files

      individual_files.each {|t, c| update_stylesheet(t, c)}

      @checked_for_updates = true
      template_locations.zip(css_locations).each do |template_location, css_location|

        Dir.glob(File.join(template_location, "**", "*.sass")).each do |file|
          # Get the relative path to the file with no extension
          name = file.sub(template_location.sub(/\/*$/, '/'), "")[0...-5]

          next if forbid_update?(name)

          filename = template_filename(name, template_location)
          css = css_filename(name, css_location)
          if options[:always_update] || stylesheet_needs_update?(name, template_location, css_location)
            update_stylesheet filename, css
          else
            run_not_updating_stylesheet filename, css
          end
        end
      end
    end

    # Watches the template directory (or directories)
    # and updates the CSS files whenever the related Sass files change.
    # `watch` never returns.
    #
    # Whenever a change is detected to a Sass file in
    # {file:SASS_REFERENCE.md#template_location-option `:template_location`},
    # the corresponding CSS file in {file:SASS_REFERENCE.md#css_location-option `:css_location`}
    # will be recompiled.
    # The CSS files of any Sass files that import the changed file will also be recompiled.
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
    #   The first string in each pair is the location of the Sass file,
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

      # TODO: Keep better track of what depends on what
      # so we don't have to run a global update every time anything changes.
      FSSM.monitor do |mon|
        template_locations.zip(css_locations).each do |template_location, css_location|
          mon.path template_location do |path|
            path.glob '**/*.sass'

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
              css = File.join(css_location, relative.gsub(/\.sass$/, '.css'))
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
