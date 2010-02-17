require 'sass/plugin'

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
        self.class.disable_native_plugin!
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

      # Disable the native Rails or Merb plugins, if they're enabled.
      # This is automatically done once the Rack plugin is activated.
      # This is done so that the stylesheets aren't checked twice for each request.
      def self.disable_native_plugin!
        if defined?(Merb::Rack) && defined?(Merb::Rack::Application) &&
            Haml::Util.has?(:instance_method, Merb::Rack::Application, :call_without_sass)
          Merb::Rack::Application.instance_eval {alias_method :call, :call_without_sass}
        end

        if defined?(ActionDispatch::Callbacks) && defined?(ActionDispatch::Callbacks.to_prepare)
          ActionDispatch::Callbacks.skip_callback(:prepare, :__sass_process)
        elsif defined?(ActionController::Base) &&
            Haml::Util.has?(:instance_method, ActionController::Base, :sass_old_process)
          ActionController::Base.instance_eval {alias_method :process, :sass_old_process}
        end
      end
    end
  end
end
