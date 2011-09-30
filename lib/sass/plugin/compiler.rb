require 'fileutils'

require 'sass'
# XXX CE: is this still necessary now that we have the compiler class?
require 'sass/callbacks'
require 'sass/plugin/configuration'
require 'sass/plugin/staleness_checker'

module Sass::Plugin

  # The Compiler class handles compilation of multiple files and/or directories,
  # including checking which CSS files are out-of-date and need to be updated
  # and calling Sass to perform the compilation on those files.
  #
  # {Sass::Plugin} uses this class to update stylesheets for a single application.
  # Unlike {Sass::Plugin}, though, the Compiler class has no global state,
  # and so multiple instances may be created and used independently.
  #
  # If you need to compile a Sass string into CSS,
  # please see the {Sass::Engine} class.
  #
  # Unlike {Sass::Plugin}, this class doesn't keep track of
  # whether or how many times a stylesheet should be updated.
  # Therefore, the following `Sass::Plugin` options are ignored by the Compiler:
  #
  # * `:never_update`
  # * `:always_check`
  class Compiler
    include Sass::Util
    include Configuration
    extend Sass::Callbacks

    # Creates a new compiler.
    #
    # @param options [{Symbol => Object}]
    #   See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    def initialize(options = {})
      self.options.merge!(options)
    end

    # Register a callback to be run after stylesheets are mass-updated.
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

    # Register a callback to be run after a single stylesheet is updated.
    # The callback is only run if the stylesheet is really updated;
    # if the CSS file is fresh, this won't be run.
    #
    # Even if the \{file:SASS_REFERENCE.md#full_exception-option `:full_exception` option}
    # is enabled, this callback won't be run
    # when an exception CSS file is being written.
    # To run an action for those files, use \{#on\_compilation\_error}.
    #
    # @yield [template, css]
    # @yieldparam template [String]
    #   The location of the Sass/SCSS file being updated.
    # @yieldparam css [String]
    #   The location of the CSS file being generated.
    define_callback :updated_stylesheet

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
    #   The location of the Sass/SCSS file being updated.
    # @yieldparam css [String]
    #   The location of the CSS file being generated.
    define_callback :updating_stylesheet

    def on_updating_stylesheet_with_deprecation_warning(&block)
      Sass::Util.sass_warn("Sass::Compiler#on_updating_stylesheet callback is deprecated and will be removed in a future release. Use Sass::Compiler#on_updated_stylesheet instead, which is run after stylesheet compilation.")
      on_updating_stylesheet_without_deprecation_warning(&block)
    end
    alias_method :on_updating_stylesheet_without_deprecation_warning, :on_updating_stylesheet
    alias_method :on_updating_stylesheet, :on_updating_stylesheet_with_deprecation_warning

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
    #   The location of the Sass/SCSS file not being updated.
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
    #   The location of the Sass/SCSS file being updated.
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
    # This happens when the corresponding Sass/SCSS file has been deleted.
    #
    # @yield [filename]
    # @yieldparam filename [String]
    #   The location of the CSS file that was deleted.
    define_callback :deleting_css

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
      Sass::Plugin.checked_for_updates = true
      staleness_checker = StalenessChecker.new(engine_options)

      template_location_array.each do |template_location, css_location|
        Dir.glob(File.join(template_location, "**", "[^_]*.s[ca]ss")).sort.each do |file|
          # Get the relative path to the file
          name = file.sub(template_location.to_s.sub(/\/*$/, '/'), "")
          css = css_filename(name, css_location)
          individual_files << [file, css]
        end
      end

      run_updating_stylesheets individual_files

      individual_files.each do |file, css|
        if options[:always_update] || staleness_checker.stylesheet_needs_update?(css, file)
          update_stylesheet(file, css)
        else
          run_not_updating_stylesheet(file, css)
        end
      end
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
        dir = Sass::Util.scope("vendor/fssm/lib")
        if $LOAD_PATH.include?(dir)
          e.message << "\n" <<
            if File.exists?(scope(".git"))
              'Run "git submodule update --init" to get the recommended version.'
            else
              'Run "gem install fssm" to get it.'
            end
          raise e
        else
          $LOAD_PATH.unshift dir
          retry
        end
      end

      unless individual_files.empty? && FSSM::Backends::Default.name == "FSSM::Backends::FSEvents"
        # As of FSSM 0.1.4, it doesn't support FSevents on individual files,
        # but it also isn't smart enough to switch to polling itself.
        require 'fssm/backends/polling'
        Sass::Util.silence_warnings do
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

    # Non-destructively modifies \{#options} so that default values are properly set,
    # and returns the result.
    #
    # @param additional_options [{Symbol => Object}] An options hash with which to merge \{#options}
    # @return [{Symbol => Object}] The modified options hash
    def engine_options(additional_options = {})
      opts = options.merge(additional_options)
      opts[:load_paths] = load_paths(opts)
      opts
    end

    # Compass expects this to exist
    def stylesheet_needs_update?(css_file, template_file)
      StalenessChecker.stylesheet_needs_update?(css_file, template_file)
    end

    private

    def update_stylesheet(filename, css)
      dir = File.dirname(css)
      unless File.exists?(dir)
        run_creating_directory dir
        FileUtils.mkdir_p dir
      end

      begin
        File.read(filename) unless File.readable?(filename) # triggers an error for handling
        engine_opts = engine_options(:css_filename => css, :filename => filename)
        result = Sass::Engine.for_file(filename, engine_opts).render
      rescue Exception => e
        compilation_error_occured = true
        run_compilation_error e, filename, css
        result = Sass::SyntaxError.exception_to_css(e, options)
      else
        run_updating_stylesheet filename, css
      end

      write_file(css, result)
      run_updated_stylesheet(filename, css) unless compilation_error_occured
    end

    def write_file(css, content)
      flag = 'w'
      flag = 'wb' if Sass::Util.windows? && options[:unix_newlines]
      File.open(css, flag) do |file|
        file.set_encoding(content.encoding) unless Sass::Util.ruby1_8?
        file.print(content)
      end
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
  end
end
