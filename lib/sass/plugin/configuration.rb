# We keep configuration in its own self-contained file
# so that we can load it independently in Rails 3,
# where the full plugin stuff is lazy-loaded.

require 'sass/callbacks'

module Sass
  module Plugin
    include Sass::Callbacks
    extend self

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
    #   The location of the Sass/SCSS file being updated.
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

    @options = {
      :css_location       => './public/stylesheets',
      :always_update      => false,
      :always_check       => true,
      :full_exception     => true
    }

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

    # Adds a new template-location/css-location mapping.
    # This means that Sass/SCSS files in `template_location`
    # will be compiled to CSS files in `css_location`.
    #
    # This is preferred over manually manipulating the {file:SASS_REFERENCE.md#template_location-option `:template_location` option}
    # since the option can be in multiple formats.
    #
    # Note that this method will change `options[:template_location]`
    # to be in the Array format.
    # This means that even if `options[:template_location]`
    # had previously been a Hash or a String,
    # it will now be an Array.
    #
    # @param template_location [String] The location where Sass/SCSS files will be.
    # @param css_location [String] The location where compiled CSS files will go.
    def add_template_location(template_location, css_location = options[:css_location])
      normalize_template_location!
      template_location_array << [template_location, css_location]
    end

    # Removes a template-location/css-location mapping.
    # This means that Sass/SCSS files in `template_location`
    # will no longer be compiled to CSS files in `css_location`.
    #
    # This is preferred over manually manipulating the {file:SASS_REFERENCE.md#template_location-option `:template_location` option}
    # since the option can be in multiple formats.
    #
    # Note that this method will change `options[:template_location]`
    # to be in the Array format.
    # This means that even if `options[:template_location]`
    # had previously been a Hash or a String,
    # it will now be an Array.
    #
    # @param template_location [String]
    #   The location where Sass/SCSS files were,
    #   which is now going to be ignored.
    # @param css_location [String]
    #   The location where compiled CSS files went, but will no longer go.
    # @return [Boolean]
    #   Non-`nil` if the given mapping already existed and was removed,
    #   or `nil` if nothing was changed.
    def remove_template_location(template_location, css_location = options[:css_location])
      normalize_template_location!
      template_location_array.delete([template_location, css_location])
    end

    # Returns the template locations configured for Sass
    # as an array of `[template_location, css_location]` pairs.
    # See the {file:SASS_REFERENCE.md#template_location-option `:template_location` option}
    # for details.
    #
    # @return [Array<(String, String)>]
    #   An array of `[template_location, css_location]` pairs.
    def template_location_array
      old_template_location = options[:template_location]
      normalize_template_location!
      options[:template_location]
    ensure
      options[:template_location] = old_template_location
    end

    private

    def normalize_template_location!
      return if options[:template_location].is_a?(Array)
      options[:template_location] =
        case options[:template_location]
        when nil; [[File.join(options[:css_location], 'sass'), options[:css_location]]]
        when String; [[options[:template_location], options[:css_location]]]
        else; options[:template_location].to_a
        end
    end
  end
end
