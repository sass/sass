# Rails 3.0.0.beta.2+
if defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load)
  require 'haml/template/options'
  require 'sass/plugin/configuration'
  ActiveSupport.on_load(:action_view) do
    if Rails.application
      Haml.init_rails(binding)
    else
      # I can't believe we have to do this, but we do.
      # Rails 3's lovely lazy-loading means that it's possible to load ActionView
      # before the application has even begin loading.
      # This means that Rails.root doesn't exist yet.
      # So if the application isn't loaded, we use this arcane initializer stuff
      # to load Haml/Sass *after* the application loads.
      #
      # Of course, it's also possible that the application is loaded before ActionView,
      # so we can't *just* rely on this method of loading.
      #
      # Ugh.
      module Haml
        class Railtie < Rails::Railtie
          initializer :haml do
            Haml.init_rails(binding)
          end
        end
      end
    end
  end
end
