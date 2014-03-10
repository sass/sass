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
    include Configuration
    extend Sass::Callbacks

    # Creates a new compiler.
    #
    # @param opts [{Symbol => Object}]
    #   See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    def initialize(opts = {})
      options.merge!(opts)
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
    # @yield [template, css, sourcemap]
    # @yieldparam template [String]
    #   The location of the Sass/SCSS file being updated.
    # @yieldparam css [String]
    #   The location of the CSS file being generated.
    # @yieldparam sourcemap [String]
    #   The location of the sourcemap being generated, if any.
    define_callback :updated_stylesheet

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

    # Register a callback to be run when Sass deletes a sourcemap file.
    # This happens when the corresponding Sass/SCSS file has been deleted.
    #
    # @yield [filename]
    # @yieldparam filename [String]
    #   The location of the sourcemap file that was deleted.
    define_callback :deleting_sourcemap

    # Updates out-of-date stylesheets.
    #
    # Checks each Sass/SCSS file in
    # {file:SASS_REFERENCE.md#template_location-option `:template_location`}
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
      individual_files = individual_files.dup
      Sass::Plugin.checked_for_updates = true
      staleness_checker = StalenessChecker.new(engine_options)

      template_location_array.each do |template_location, css_location|
        Sass::Util.glob(File.join(template_location, "**", "[^_]*.s[ca]ss")).sort.each do |file|
          # Get the relative path to the file
          name = file.sub(template_location.to_s.sub(/\/*$/, '/'), "")
          css = css_filename(name, css_location)
          sourcemap = Sass::Util.sourcemap_name(css) if engine_options[:sourcemap]
          individual_files << [file, css, sourcemap]
        end
      end

      individual_files.each do |file, css, sourcemap|
        # TODO: Does staleness_checker need to check the sourcemap file as well?
        if options[:always_update] || staleness_checker.stylesheet_needs_update?(css, file)
          update_stylesheet(file, css, sourcemap)
        else
          run_not_updating_stylesheet(file, css, sourcemap)
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
    # Note that `watch` uses the [Listen](http://github.com/guard/listen) library
    # to monitor the filesystem for changes.
    # Listen isn't loaded until `watch` is run.
    # The version of Listen distributed with Sass is loaded by default,
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

      directories = watched_paths
      individual_files.each do |(source, _, _)|
        directories << File.dirname(File.expand_path(source))
      end
      directories = remove_redundant_directories(directories)

      # A Listen version prior to 2.0 will write a test file to a directory to
      # see if a watcher supports watching that directory. That breaks horribly
      # on read-only directories, so we filter those out.
      directories.reject {|d| File.writable?(d)} unless Sass::Util.listen_geq_2?

      # TODO: Keep better track of what depends on what
      # so we don't have to run a global update every time anything changes.
      listener_args = directories + [{:relative_paths => false}]

      # The native windows listener is much slower than the polling option, according to
      # https://github.com/nex3/sass/commit/a3031856b22bc834a5417dedecb038b7be9b9e3e
      poll = @options[:poll] || Sass::Util.windows?
      if poll && Sass::Util.listen_geq_2?
        # In Listen 2.0.0 and on, :force_polling is an option. In earlier
        # versions, it's a method on the listener (called below).
        listener_args.last[:force_polling] = true
      end

      listener = create_listener(*listener_args) do |modified, added, removed|
        on_file_changed(individual_files, modified, added, removed)
      end

      if poll && !Sass::Util.listen_geq_2?
        # In Listen 2.0.0 and on, :force_polling is an option (set above). In
        # earlier versions, it's a method on the listener.
        listener.force_polling(true)
      end

      listen_to(listener)
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

    def create_listener(*args, &block)
      load_listen!
      if Sass::Util.listen_geq_2?
        Listen.to(*args, &block)
      else
        Listen::Listener.new(*args, &block)
      end
    end

    def listen_to(listener)
      if Sass::Util.listen_geq_2?
        listener.start.join
      else
        listener.start!
      end
    rescue Interrupt
      # Squelch Interrupt for clean exit from Listen::Listener
    end

    def remove_redundant_directories(directories)
      dedupped = []
      directories.each do |new_directory|
        # no need to add a directory that is already watched.
        next if dedupped.any? do |existing_directory|
          child_of_directory?(existing_directory, new_directory)
        end
        # get rid of any sub directories of this new directory
        dedupped.reject! do |existing_directory|
          child_of_directory?(new_directory, existing_directory)
        end
        dedupped << new_directory
      end
      dedupped
    end

    def load_listen!
      if defined?(gem)
        begin
          gem 'listen', '>= 1.1.0', '< 3.0.0'
          require 'listen'
        rescue Gem::LoadError
          dir = Sass::Util.scope("vendor/listen/lib")
          $LOAD_PATH.unshift dir
          begin
            require 'listen'
          rescue LoadError => e
            if Sass::Util.version_geq(RUBY_VERSION, "1.9.3")
              version_constraint = "~> 2.7"
            else
              version_constraint = "~> 1.1"
            end
            e.message << "\n" <<
              "Run \"gem install listen --version '#{version_constraint}'\" to get it."
            raise e
          end
        end
      else
        begin
          require 'listen'
        rescue LoadError => e
          dir = Sass::Util.scope("vendor/listen/lib")
          if $LOAD_PATH.include?(dir)
            raise e unless File.exists?(scope(".git"))
            e.message << "\n" <<
              'Run "git submodule update --init" to get the bundled version.'
          else
            $LOAD_PATH.unshift dir
            retry
          end
        end
      end
    end

    def on_file_changed(individual_files, modified, added, removed)
      recompile_required = false

      modified.uniq.each do |f|
        next unless watched_file?(f)
        recompile_required = true
        run_template_modified(relative_to_pwd(f))
      end

      added.uniq.each do |f|
        next unless watched_file?(f)
        recompile_required = true
        run_template_created(relative_to_pwd(f))
      end

      removed.uniq.each do |f|
        if (files = individual_files.find {|(source, _, _)| File.expand_path(source) == f})
          recompile_required = true
          # This was a file we were watching explicitly and compiling to a particular location.
          # Delete the corresponding file.
          try_delete_css files[1]
        else
          next unless watched_file?(f)
          recompile_required = true
          # Look for the sass directory that contained the sass file
          # And try to remove the css file that corresponds to it
          template_location_array.each do |(sass_dir, css_dir)|
            sass_dir = File.expand_path(sass_dir)
            if child_of_directory?(sass_dir, f)
              remainder = f[(sass_dir.size + 1)..-1]
              try_delete_css(css_filename(remainder, css_dir))
              break
            end
          end
        end
        run_template_deleted(relative_to_pwd(f))
      end

      if recompile_required
        # In case a file we're watching is removed and then recreated we
        # prune out the non-existant files here.
        watched_files_remaining = individual_files.select {|(source, _, _)| File.exists?(source)}
        update_stylesheets(watched_files_remaining)
      end
    end

    def update_stylesheet(filename, css, sourcemap)
      dir = File.dirname(css)
      unless File.exists?(dir)
        run_creating_directory dir
        FileUtils.mkdir_p dir
      end

      begin
        File.read(filename) unless File.readable?(filename) # triggers an error for handling
        engine_opts = engine_options(:css_filename => css,
                                     :filename => filename,
                                     :sourcemap_filename => sourcemap)
        mapping = nil
        engine = Sass::Engine.for_file(filename, engine_opts)
        if sourcemap
          rendered, mapping = engine.render_with_sourcemap(File.basename(sourcemap))
        else
          rendered = engine.render
        end
      rescue StandardError => e
        compilation_error_occured = true
        run_compilation_error e, filename, css, sourcemap
        rendered = Sass::SyntaxError.exception_to_css(e, options)
      end

      write_file(css, rendered)
      if mapping
        write_file(sourcemap, mapping.to_json(:css_path => css, :sourcemap_path => sourcemap))
      end
      run_updated_stylesheet(filename, css, sourcemap) unless compilation_error_occured
    end

    def write_file(fileName, content)
      flag = 'w'
      flag = 'wb' if Sass::Util.windows? && options[:unix_newlines]
      File.open(fileName, flag) do |file|
        file.set_encoding(content.encoding) unless Sass::Util.ruby1_8?
        file.print(content)
      end
    end

    def try_delete_css(css)
      if File.exists?(css)
        run_deleting_css css
        File.delete css
      end
      map = Sass::Util.sourcemap_name(css)
      if File.exists?(map)
        run_deleting_sourcemap map
        File.delete map
      end
    end

    def watched_file?(file)
      normalized_load_paths.find {|lp| lp.watched_file?(file)}
    end

    def watched_paths
      @watched_paths ||= normalized_load_paths.map {|lp| lp.directories_to_watch}.compact.flatten
    end

    def normalized_load_paths
      @normalized_load_paths ||=
        Sass::Engine.normalize_options(:load_paths => load_paths)[:load_paths]
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
      "#{path}#{File::SEPARATOR unless path.end_with?(File::SEPARATOR)}#{name}".
        gsub(/\.s[ac]ss$/, '.css')
    end

    def relative_to_pwd(f)
      Sass::Util.pathname(f).relative_path_from(Sass::Util.pathname(Dir.pwd)).to_s
    rescue ArgumentError # when a relative path cannot be computed
      f
    end

    def child_of_directory?(parent, child)
      parent_dir = parent.end_with?(File::SEPARATOR) ? parent : (parent + File::SEPARATOR)
      child.start_with?(parent_dir) || parent == child
    end
  end
end
