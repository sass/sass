# This file is here to integrate with Rails 3,
# since there's no better way to do so as of 14 March 2010.
# Yehuda promises there will be soon,
# and once there is we should switch to that.

# Rails 3.0.0.beta.2+
if defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load)
  require 'haml/template/options'
  require 'sass/plugin/configuration'
  ActiveSupport.on_load(:action_view) {Haml.init_rails(binding)}
end
