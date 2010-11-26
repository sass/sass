unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  module Sass::Plugin::Configuration
    # Different default options in a rails envirionment.
    def default_options
      return @default_options if @default_options
      opts = {
        :quiet             => Sass::Util.rails_env != "production",
        :full_exception    => Sass::Util.rails_env != "production",
        :cache_location    => Sass::Util.rails_root + '/tmp/sass-cache'
      }

      if Sass::Util.ap_geq?('3.1.0.beta')
        require 'sass/importers/rails'
        require 'sass/cache_stores/active_support'
        opts.merge!(:load_paths => [Sass::Importers::Rails.new])
        opts.merge!(:cache_store => Sass::CacheStores::ActiveSupport.new(Rails.cache)) if Rails.cache
      else
        opts.merge!(
          :always_update      => false,
          :template_location => Sass::Util.rails_root + '/public/stylesheets/sass',
          :css_location      => Sass::Util.rails_root + '/public/stylesheets',
          :always_check      => Sass::Util.rails_env == "development")
      end

      @default_options = opts.freeze
    end
  end

  Sass::Plugin.options.reverse_merge!(Sass::Plugin.default_options)

  # Disable this for now, until we figure out how to get Rails
  # to pass in the view.
  if Sass::Util.ap_geq?('3.1.0.beta')
    class Sass::Plugin::TemplateHandler
      attr_reader :syntax

      def initialize(syntax)
        @syntax = syntax
      end

      def handles_encoding?; true; end

      def call(template, view)
        engine = Sass::Engine.new(template.source,
          Sass::Plugin.engine_options.merge(
            :syntax => @syntax,
            :filename => template.virtual_path,
            :_rails_lookup_context => view.lookup_context,
            :importer => Sass::Importers::Rails.new))

        template.data[:sass_importers] = engine.dependencies.map do |e|
          [e.options[:filename], e.options[:importer]]
        end

        stylesheet =
          begin
            engine.render
          rescue Sass::SyntaxError => e
            Sass::Plugin::TemplateHandler.munge_exception e, view.lookup_context
            Sass::SyntaxError.exception_to_css(e, Sass::Plugin.engine_options)
          end

        <<RUBY
begin
  if Sass::Plugin::TemplateHandler.dependencies_changed?(
      @_template.data[:sass_importers], #{Time.now.to_i}, lookup_context)
    @_template.expire!
    @_template.rerender(self)
  else
    #{stylesheet.inspect}
  end
rescue Sass::SyntaxError => e
  Sass::Plugin::TemplateHandler.munge_exception e, lookup_context
  Sass::SyntaxError.exception_to_css(e, Sass::Plugin.engine_options)
end
RUBY
      end

      def self.dependencies_changed?(deps, since, lookup_context)
        opts = Sass::Plugin.engine_options.merge(:_rails_lookup_context => lookup_context)
        deps.any? do |d, i|
          return true unless time = i.mtime(d, opts)
          time.to_i > since
        end
      end

      def self.munge_exception(e, lookup_context)
        importer = Sass::Importers::Rails.new
        opts = Sass::Plugin.engine_options.merge(:_rails_lookup_context => lookup_context)
        e.sass_backtrace.each do |bt|
          next unless engine = importer.find(bt[:filename], opts)
          bt[:filename] = engine.options[:_rails_filename]
        end
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
