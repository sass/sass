require 'sass/plugin'

module Sass
  module Plugin
    class Rack
      def initialize(app)
        @app = app
      end

      def call(env)
        Sass::Plugin.check_for_updates
        @app.call(env)
      end
    end
  end
end
