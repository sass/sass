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
                              :quiet             => env != "production",
                              :full_exception    => env != "production")
  config = Merb::Plugins.config[:sass] || Merb::Plugins.config["sass"] || {}

  if defined? config.symbolize_keys!
    config.symbolize_keys!
  end

  Sass::Plugin.options.merge!(config)

  require 'sass/plugin/rack'
  Merb::Config[:app].use Sass::Plugin::Rack
end
