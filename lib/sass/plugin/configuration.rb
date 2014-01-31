module Sass
  module Plugin
    # We keep configuration in its own self-contained file
    # so that we can load it independently in Rails 3,
    # where the full plugin stuff is lazy-loaded.
    module Configuration
      # Returns the default options for a {Sass::Plugin::Compiler}.
      #
      # @return [{Symbol => Object}]
      def default_options
        @default_options ||= {
          :css_location       => './public/stylesheets',
          :always_update      => false,
          :always_check       => true,
          :full_exception     => true,
          :cache_location     => ".sass-cache"
        }.freeze
      end

      # Resets the options and
      # {Sass::Callbacks::InstanceMethods#clear_callbacks! clears all callbacks}.
      def reset!
        @options = nil
        clear_callbacks!
      end

      # An options hash.
      # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
      #
      # @return [{Symbol => Object}]
      def options
        @options ||= default_options.dup
      end

      # Adds a new template-location/css-location mapping.
      # This means that Sass/SCSS files in `template_location`
      # will be compiled to CSS files in `css_location`.
      #
      # This is preferred over manually manipulating the
      # {file:SASS_REFERENCE.md#template_location-option `:template_location` option}
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
      # This is preferred over manually manipulating the
      # {file:SASS_REFERENCE.md#template_location-option `:template_location` option}
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
          when nil
            if options[:css_location]
              [[File.join(options[:css_location], 'sass'), options[:css_location]]]
            else
              []
            end
          when String
            [[options[:template_location], options[:css_location]]]
          else
            options[:template_location].to_a
          end
      end
    end
  end
end
