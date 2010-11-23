unless defined?(Sass::RAILS_LOADED)
  Sass::RAILS_LOADED = true

  module Sass::Plugin::Configuration
    # Different default options in a rails envirionment.
    def default_options
      opts = {
        :quiet             => Sass::Util.rails_env != "production",
        :full_exception    => Sass::Util.rails_env != "production"
      }

      if Sass::Util.ap_geq?('3.1.0.beta')
        opts.merge!(:cache => false, :load_paths => [])
      else
        opts.merge!(
          :always_update      => false,
          :template_location => Sass::Util.rails_root + '/public/stylesheets/sass',
          :css_location      => Sass::Util.rails_root + '/public/stylesheets',
          :cache_location    => Sass::Util.rails_root + '/tmp/sass-cache',
          :always_check      => Sass::Util.rails_env == "development")
      end

      @default_options ||= opts.freeze
    end
  end

  Sass::Plugin.options.reverse_merge!(Sass::Plugin.default_options)

  # Disable this for now, until we figure out how to get Rails
  # to pass in the view.
  if Sass::Util.ap_geq?('3.1.0.beta')
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
          Sass::Plugin.engine_options.merge(
            :syntax => @syntax,
            :filename => template.virtual_path,
            :importer => rails_importer,
            :load_paths => [rails_importer] + Sass::Plugin.engine_options[:load_paths]))

        # We need to serialize/deserialize the importers to make sure
        # that each dependency is matched up to its proper importer
        # for when importer#mtime is called.
        dependencies = engine.dependencies
        importers = Sass::Util.to_hash(
          Sass::Util.enum_with_index(dependencies).map do |e, i|
            importer = e.options[:importer]
            [importer, {
                :variable => "importer_#{i}",
                :expression => (importer == rails_importer ?
                  "Sass::Importers::Rails.new(lookup_context)" :
                  "Sass::Util.load(#{Sass::Util.dump(importer)})")
              }]
          end)

        stylesheet =
          begin
            engine.render
          rescue Sass::SyntaxError => e
            Sass::Plugin::TemplateHandler.munge_exception e, view.lookup_context
            Sass::SyntaxError.exception_to_css(e, Sass::Plugin.engine_options)
          end

        <<RUBY
begin
  #{importers.map {|_, val| "#{val[:variable]} = #{val[:expression]}"}.join("\n")}
  if Sass::Plugin::TemplateHandler.dependencies_changed?(
      [#{dependencies.map {|e| "[#{e.options[:filename].inspect}, #{importers[e.options[:importer]][:variable]}]"}.join(',')}],
      #{Time.now.to_i})
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

      def self.dependencies_changed?(deps, since)
        deps.any? {|d, i| i.mtime(d, Sass::Plugin.engine_options) > since}
      end

      def self.munge_exception(e, lookup_context)
        importer = Sass::Importers::Rails.new(lookup_context)
        e.sass_backtrace.each do |bt|
          next unless engine = importer.find(bt[:filename], Sass::Plugin.engine_options)
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
