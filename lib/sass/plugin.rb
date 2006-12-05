require File.dirname(__FILE__) + '/../sass'
require 'sass/engine'

require 'rubygems'
require 'action_controller'

module Sass
  #Rails plugin stuff. For use with ActionView.
  module Plugin
    class << self
      @@options = {
        :template_location  => RAILS_ROOT + '/public/stylesheets/sass',
        :css_location       => RAILS_ROOT + '/public/stylesheets',
        :always_update      => false,
        :always_check       => RAILS_ENV != "production",
        :style              => :nested
      }

      # Gets various options for Sass.
      #--
      # TODO: *DOCUMENT OPTIONS*
      #++
      def options
        @@options
      end

      # Sets various options for Sass.
      def options=(value)
        @@options.merge!(value)
      end
      
      def update_stylesheets
        Dir[options[:template_location] + '/*.sass'].each do |file|
          name = File.basename(file)[0...-5]
          
          if options[:always_update] || stylesheet_needs_update?(name)
            css = css_filename(name)
            File.delete(css) if File.exists?(css)
            
            engine = Engine.new(File.read(template_filename(name)), @@options.dup)
            result = engine.render
            
            Dir.mkdir(options[:css_location]) unless File.exist?(options[:css_location])
            File.open(css, 'w') do |file|
              file.print(result)
            end
          end
        end
      end
      
      private
      
      def template_filename(name)
        "#{@@options[:template_location]}/#{name}.sass"
      end
      
      def css_filename(name)
        "#{@@options[:css_location]}/#{name}.css"
      end
      
      def stylesheet_needs_update?(name)
        !File.exists?(css_filename(name)) || (File.mtime(template_filename(name)) - 2) > File.mtime(css_filename(name))
      end
    end
  end
end

module ActionController
  class Base
    alias_method :sass_old_process, :process
    def process(*args)
      Sass::Plugin.update_stylesheets if Sass::Plugin.options[:always_update] || Sass::Plugin.options[:always_check]
      sass_old_process(*args)
    end
  end
end
