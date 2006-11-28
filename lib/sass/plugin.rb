
#Rails plugin stuff. For use with action_view

module Sass
  module Plugin

    def options
      @@options
    end

    def stylesheet_location
      @@options[:stylesheet_location] || (RAILS_ROOT + "/public/stylesheets/")
    end
    
    def sass_template(name)
      file_location = stylesheet_location + name
      if stylesheet_needs_update?(file_location)
        file = File.open(file_location + ".css")
        Sass::Engine.new.render(file_location + ".sass")
      end
    end

    def stylesheet_needs_update?(file_location)
      !File.exists?(file_location + ".css") || (File.mtime("#{file_location}.sass") - 60) > File.mtime("#{file_location}.css")
    end
  end
end
