unless defined?(Sass::MERB_LOADED)
  Sass::MERB_LOADED = true

  Sass::Plugin.options.merge!(:template_location  => MERB_ROOT + '/public/stylesheets/sass',
                              :css_location       => MERB_ROOT + '/public/stylesheets',
                              :always_check       => MERB_ENV != "production",
                              :full_exception     => MERB_ENV != "production")
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
