unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  module Sass::Plugin::Configuration
    # Different default options in a rails envirionment.
    def default_options
      @default_options ||= {
        :always_update      => false,
        :template_location => Haml::Util.rails_root + '/public/stylesheets/sass',
        :css_location      => Haml::Util.rails_root + '/public/stylesheets',
        :cache_location    => Haml::Util.rails_root + '/tmp/sass-cache',
        :always_check      => Haml::Util.rails_env == "development",
        :quiet             => Haml::Util.rails_env != "production",
        :full_exception    => Haml::Util.rails_env != "production"
      }.freeze
    end
  end

  Sass::Plugin.options.reverse_merge!(Sass::Plugin.default_options)

  if defined?(ActionController::Metal)
    # Rails >= 3.0
    require 'sass/plugin/rack'
    Rails.configuration.middleware.use(Sass::Plugin::Rack)
  elsif defined?(ActionController::Dispatcher) &&
      defined?(ActionController::Dispatcher.middleware)
    # Rails >= 2.3
    require 'sass/plugin/rack'
    ActionController::Dispatcher.middleware.use(Sass::Plugin::Rack)
  else
    module ActionController
      class Base
        alias_method :sass_old_process, :process
        def process(*args)
          Sass::Plugin.check_for_updates
          sass_old_process(*args)
        end
      end
    end
  end
end
