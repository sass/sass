unless defined?(Sass::MERB_LOADED)
  Sass::MERB_LOADED = true

  version = Merb::VERSION.split('.').map { |n| n.to_i }
  if version[0] <= 0 && version[1] < 5
    root = MERB_ROOT
    env  = MERB_ENV
  else
    root = Merb.root
    env  = Merb.environment
  end

  Sass::Plugin.options.merge!(:template_location  => root + '/public/stylesheets/sass',
                              :css_location       => root + '/public/stylesheets',
                              :always_check       => env != "production",
                              :full_exception     => env != "production")
  config = Merb::Plugins.config[:sass] || Merb::Plugins.config["sass"] || {}
  config.symbolize_keys!
  Sass::Plugin.options.merge!(config)
  
  class MerbHandler # :nodoc:
    def process_with_sass(request, response)
      Sass::Plugin.update_stylesheets if Sass::Plugin.options[:always_update] || Sass::Plugin.options[:always_check]
      process_without_sass(request, response)
    end
    alias_method :process_without_sass, :process
    alias_method :process, :process_with_sass
  end
end
