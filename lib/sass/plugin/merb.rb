unless defined?(Sass::MERB_LOADED)
  Sass::MERB_LOADED = true

  version = Merb::VERSION.split('.').map { |n| n.to_i }
  if version[0] <= 0 && version[1] < 5
    root = MERB_ROOT
    env  = MERB_ENV
  else
    root = Merb.root.to_s
    env  = Merb.environment
  end

  Sass::Plugin.options.merge!(:template_location => root + '/public/stylesheets/sass',
                              :css_location      => root + '/public/stylesheets',
                              :cache_location    => root + '/tmp/sass-cache',
                              :always_check      => env != "production",
                              :full_exception    => env != "production")
  config = Merb::Plugins.config[:sass] || Merb::Plugins.config["sass"] || {}

  if defined? config.symbolize_keys!
    config.symbolize_keys!
  end

  Sass::Plugin.options.merge!(config)

  if version[0] > 0 || version[1] >= 9

    class Merb::Rack::Application
      def call_with_sass(env)
        Sass::Plugin.check_for_updates
        call_without_sass(env)
      end
      alias_method :call_without_sass, :call
      alias_method :call, :call_with_sass
    end

  else

    class MerbHandler
      def process_with_sass(request, response)
        Sass::Plugin.check_for_updates
        process_without_sass(request, response)
      end
      alias_method :process_without_sass, :process
      alias_method :process, :process_with_sass
    end

  end
end
