unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  Sass::Plugin.options.merge!(:template_location => Haml::Util.rails_root + '/public/stylesheets/sass',
                              :css_location      => Haml::Util.rails_root + '/public/stylesheets',
                              :cache_location    => Haml::Util.rails_root + '/tmp/sass-cache',
                              :always_check      => RAILS_ENV != "production",
                              :full_exception    => RAILS_ENV != "production")

  module ActionController
    class Base
      alias_method :sass_old_process, :process
      def process(*args)
        if !Sass::Plugin.checked_for_updates ||
            Sass::Plugin.options[:always_update] || Sass::Plugin.options[:always_check]
          Sass::Plugin.update_stylesheets
        end

        sass_old_process(*args)
      end
    end
  end
end
