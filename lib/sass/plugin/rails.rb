unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  Sass::Plugin.options.merge!(:template_location => Haml::Util.rails_root + '/public/stylesheets/sass',
                              :css_location      => Haml::Util.rails_root + '/public/stylesheets',
                              :cache_location    => Haml::Util.rails_root + '/tmp/sass-cache',
                              :always_check      => RAILS_ENV != "production",
                              :full_exception    => RAILS_ENV != "production")

  if defined?(ActionDispatch::Callbacks.to_prepare)
    # Rails >= 3.0.0
    ActionDispatch::Callbacks.to_prepare {Sass::Plugin.check_for_updates}
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
