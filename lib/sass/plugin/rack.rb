module Sass
  module Plugin
    # Rack middleware for compiling Sass code.
    #
    # ## Activate
    #
    #     require 'sass/plugin/rack'
    #     use Sass::Plugin::Rack
    #
    # ## Customize
    #
    #     Sass::Plugin.options.merge(
    #       :cache_location => './tmp/sass-cache',
    #       :never_update => environment != :production,
    #       :full_exception => environment != :production)
    #
    # {file:SASS_REFERENCE.md#options See the Reference for more options}.
    #
    # ## Use
    #
    # Put your Sass files in `public/stylesheets/sass`.
    # Your CSS will be generated in `public/stylesheets`,
    # and regenerated every request if necessary.
    # The locations and frequency {file:SASS_REFERENCE.md#options can be customized}.
    # That's all there is to it!
    class Rack
      # Initialize the middleware.
      #
      # @param app [#call] The Rack application
      def initialize(app)
        @app = app
      end

      # Process a request, checking the Sass stylesheets for changes
      # and updating them if necessary.
      #
      # @param env The Rack request environment
      # @return [(#to_i, {String => String}, Object)] The Rack response
      def call(env)
        Sass::Plugin.check_for_updates
        @app.call(env)
      end
    end
  end
end

require 'sass/plugin'
