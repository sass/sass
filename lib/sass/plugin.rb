require 'sass/engine'

module Sass
  # This module contains methods to aid in using Sass
  # as a stylesheet-rendering plugin for various systems.
  # Currently Rails/ActionController and Merb are supported out of the box.
  module Plugin
    class << self
      @@options = {
        :template_location  => './public/stylesheets/sass',
        :css_location       => './public/stylesheets',
        :always_update      => false,
        :always_check       => true,
        :full_exception     => true
      }

      # Gets various options for Sass. See README for details.
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
      
      # Checks each stylesheet in <tt>options[:css_location]</tt>
      # to see if it needs updating,
      # and updates it using the corresponding template
      # from <tt>options[:templates]</tt>
      # if it does.
      def update_stylesheets
        Dir.glob(File.join(options[:template_location], "**", "*.sass")).entries.each do |file|
          
          # Get the relative path to the file with no extension
          name = file.sub(options[:template_location] + "/", "")[0...-5]
                    
          if options[:always_update] || stylesheet_needs_update?(name)
            css = css_filename(name)
            File.delete(css) if File.exists?(css)
            
            filename = template_filename(name)
            l_options = @@options.dup
            l_options[:filename] = filename
            l_options[:load_paths] = (l_options[:load_paths] || []) + [l_options[:template_location]]
            engine = Engine.new(File.read(filename), l_options)
            begin
              result = engine.render
            rescue Exception => e
              if options[:full_exception]
                e_string = "#{e.class}: #{e.message}"

                if e.is_a? Sass::SyntaxError
                  e_string << "\non line #{e.sass_line}"

                  if e.sass_filename
                    e_string << " of #{e.sass_filename}"

                    if File.exists?(e.sass_filename)
                      e_string << "\n\n"

                      min = [e.sass_line - 5, 0].max
                      File.read(e.sass_filename).rstrip.split("\n")[
                          min .. e.sass_line + 5
                      ].each_with_index do |line, i|
                        e_string << "#{min + i + 1}: #{line}\n"
                      end
                    end
                  end
                end
                result = "/*\n#{e_string}\n\nBacktrace:\n#{e.backtrace.join("\n")}\n*/"
              else
                result = "/* Internal stylesheet error */"
              end
            end
            
            # Create any directories that might be necessary
            dirs = [l_options[:css_location]]
            name.split("/")[0...-1].each { |dir| dirs << "#{dirs[-1]}/#{dir}" }
            dirs.each { |dir| Dir.mkdir(dir) unless File.exist?(dir) }
            
            # Finally, write the file
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

require 'sass/plugin/rails' if defined?(ActionController)
require 'sass/plugin/merb'  if defined?(Merb::Plugins)
