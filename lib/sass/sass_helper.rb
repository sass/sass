require File.dirname(__FILE__) + '/../sass'
require 'sass/engine'

#Rails plugin stuff. For use with action_view

module Sass
  module SassHelper
    @@options = {}

    class << self
      def options; @@options; end

      def _stylesheet_location
        @@options[:stylesheet_location] || (RAILS_ROOT + "/public/stylesheets/")
      end

      def _always_update
        @@options[:always_update] || false
      end

      def stylesheet_needs_update?(file_location)
        !File.exists?(file_location + ".css") || (File.mtime("#{file_location}.sass") - 2) > File.mtime("#{file_location}.css")
      end
    end

    def sass_template(name)
      file_location = SassHelper._stylesheet_location + "/" + name.to_s
      if SassHelper._always_update || SassHelper.stylesheet_needs_update?(file_location)
        output_file = File.open(file_location + ".css", "w+")
        output_file << Sass::Engine.new.render_file(file_location + ".sass")
        output_file.close
      end
    end
  end
end
