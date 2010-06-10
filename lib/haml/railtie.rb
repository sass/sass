# Rails 3.0.0.beta.2+
if defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load)
  require 'haml/template/options'
  require 'sass/plugin/configuration'
  ActiveSupport.on_load(:before_initialize) do
    require 'sass'
    require 'sass/plugin'

    # Haml requires AV, but Sass doesn't
    ActiveSupport.on_load(:action_view) do
      Haml.init_rails(binding)
    end
  end
end
