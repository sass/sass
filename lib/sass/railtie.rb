# Rails 3.0.0.beta.2+
if defined?(ActiveSupport) && Sass::Util.has?(:public_method, ActiveSupport, :on_load)
  require 'sass/plugin/configuration'
  ActiveSupport.on_load(:before_initialize) do
    require 'sass'
    require 'sass/plugin'
  end
end
