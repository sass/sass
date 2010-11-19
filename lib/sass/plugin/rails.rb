unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  module Sass::Plugin::Configuration
    # Different default options in a rails envirionment.
    def default_options
      @default_options ||= {
        :always_update      => false,
        :template_location => Sass::Util.rails_root + '/public/stylesheets/sass',
        :css_location      => Sass::Util.rails_root + '/public/stylesheets',
        :cache_location    => Sass::Util.rails_root + '/tmp/sass-cache',
        :always_check      => Sass::Util.rails_env == "development",
        :quiet             => Sass::Util.rails_env != "production",
        :full_exception    => Sass::Util.rails_env != "production"
      }.freeze
    end
  end

  Sass::Plugin.options.reverse_merge!(Sass::Plugin.default_options)

  # Disable this for now, until we figure out how to get Rails
  # to pass in the view.
  if false #Sass::Util.ap_geq?('3.1.0.beta')
    require 'sass/importers/rails'
    class Sass::Plugin::TemplateHandler
      attr_reader :syntax

      def initialize(syntax)
        @syntax = syntax
      end

      def handles_encoding?; true; end

      def call(template, view)
        rails_importer = Sass::Importers::Rails.new(view.lookup_context)
        engine = Sass::Engine.new(template.source,
          :syntax => @syntax,
          :cache => false,
          :filename => template.virtual_path,
          :importer => rails_importer,
          :load_paths => [rails_importer])

        dependencies = engine.dependencies
        <<RUBY
if Sass::Plugin::TemplateHandler.dependencies_changed?(
    #{dependencies.map {|e| e.options[:filename]}.inspect},
    Sass::Importers::Rails.new(lookup_context),
    #{Time.now.to_i})
  @_template.expire!
  @_template.rerender(self)
else
  #{engine.render.inspect}
end
RUBY
      end

      def self.dependencies_changed?(deps, importer, since)
        options = Sass::Plugin.engine_options.merge(
          :load_paths => [importer],
          :cache => false)
        deps.any? {|d| importer.mtime(d, options) > since}
      end
    end

    ActionView::Template.register_template_handler(:sass, Sass::Plugin::TemplateHandler.new(:sass))
    ActionView::Template.register_template_handler(:scss, Sass::Plugin::TemplateHandler.new(:scss))
  elsif defined?(ActionController::Metal)
    # Rails >= 3.0
    require 'sass/plugin/rack'
    Rails.configuration.middleware.use(Sass::Plugin::Rack)
  elsif defined?(ActionController::Dispatcher) &&
      defined?(ActionController::Dispatcher.middleware)
    # Rails >= 2.3
    require 'sass/plugin/rack'
    ActionController::Dispatcher.middleware.use(Sass::Plugin::Rack)
  else
    module ActionController
      class Base
        alias_method :sass_old_process, :process
        def process(*args)
          Sass::Plugin.check_for_updates
          sass_old_process(*args)
        end
      end
    end
  end
end
