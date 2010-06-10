unless defined?(Sass::RAILS_LOADED)
  if Haml::Util.ap_geq_3? && !Haml::Util.ap_geq?("3.0.0.beta4")
    Haml::Util.haml_warn(<<WARNING)
DEPRECATION WARNING:
Haml/Sass support for Rails 3 versions before beta 4 is deprecated,
  and will be removed in Haml/Sass 3.0.13.
  Please upgrade to Rails 3.0.0.beta4 or later.
WARNING
  end

  Sass::RAILS_LOADED = true

  # Reverse-merging (we're in Rails, anyway) so we dont' clobber what's already been defined further up-stream
  Sass::Plugin.options.reverse_merge!(:template_location => Haml::Util.rails_root + '/public/stylesheets/sass',
                                      :css_location      => Haml::Util.rails_root + '/public/stylesheets',
                                      :cache_location    => Haml::Util.rails_root + '/tmp/sass-cache',
                                      :always_check      => Haml::Util.rails_env == "development",
                                      :quiet             => Haml::Util.rails_env != "production",
                                      :full_exception    => Haml::Util.rails_env != "production")

  if defined?(ActionController::Metal)
    # Rails >= 3.0
    require 'sass/plugin/rack'
    if Rails.application.instance_variable_get('@app')
      # The application has already been built,
      # so we need to hack the middleware in
      Rails.application.instance_variable_set('@app',
        Sass::Plugin::Rack.new(Rails.application.app))
    else
      # The application hasn't been built yet,
      # so we can just add Sass::Plugin::Rack
      # to the pending middleware stack.
      Rails.configuration.middleware.use(Sass::Plugin::Rack)
    end
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
