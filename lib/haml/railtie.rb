# This file is here to integrate with Rails 3,
# since there's no better way to do so as of 14 March 2010.
# Yehuda promises there will be soon,
# and once there is we should switch to that.

if defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load)
  # Rails 3.0.0.beta.2+
  ActiveSupport.on_load(:action_view) {Haml.init_rails(binding)}
elsif defined?(Rails::Railtie)
  # Rails 3.0.0.beta1
  module Haml
    class Railtie < Rails::Railtie
      initializer :haml do
        Haml.init_rails(binding)
      end
    end
  end
end
