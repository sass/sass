unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  module Sass::Plugin::Configuration
    # Different default options in a rails envirionment.
    def default_options
      return @default_options if @default_options
      opts = {
        :quiet             => Sass::Util.rails_env != "production",
        :full_exception    => Sass::Util.rails_env != "production",
        :cache_location    => Sass::Util.rails_root + '/tmp/sass-cache',
        :always_update     => false,
        :template_location => Sass::Util.rails_root + '/public/stylesheets/sass',
        :css_location      => Sass::Util.rails_root + '/public/stylesheets',
        :always_check      => Sass::Util.rails_env == "development",
        # This is bad. See Sass::Plugin::Configuration#default_options_default_proc.
        # :cache_store     => Sass::CacheStores::Filesystem.new(Sass::Plugin.options[:cache_location])
      }

      @default_options.default_proc = Sass::Plugin::Configuration.default_options_default_proc
      @default_options = opts.freeze
    end
  end

  Sass::Plugin.options.reverse_merge!(Sass::Plugin.default_options)

  # Rails 3.1 loads and handles Sass all on its own
  if defined?(ActionController::Metal)
    # 3.1 > Rails >= 3.0
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
