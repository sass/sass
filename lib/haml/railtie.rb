if defined?(Rails::Railtie)
  module Haml
    class Railtie < Rails::Railtie
      initializer :haml do
        Haml.init_rails(binding)
      end
    end
  end
end
