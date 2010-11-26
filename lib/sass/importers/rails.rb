module Sass
  module Importers
    # An importer that wraps the Rails 3.1 view infrastructure.
    # Loads Sass files as though they were views in Rails.
    # Currently doesn't support caching.
    #
    # This is different from standard Rails rendering
    # in that Sass doesn't have a concept of importing partials
    # as a distinct action from importing other Sass files.
    # Imports within Rails behave more like Sass imports:
    # they will first attempt to find a non-partial file,
    # and failing that will fall back on a partial.
    #
    # Each importer instance is local to a single request for a single view.
    # It contains the ActionView::LookupContext for that request,
    # as well as the controller prefix for the view being generated.
    class Rails < Base
      # Creates a new Rails importer that imports files as Rails views.
      def initialize; end

      # @see Base#find_relative
      def find_relative(uri, base, options)
        find_(uri, base.split('/')[0...-1].join('/'), options)
      end

      # @see Base#find
      def find(uri, options)
        find_(uri, nil, options)
      end

      # @see Base#mtime
      def mtime(uri, options)
        return unless template =
          find_template(uri, nil, !:partial, options) ||
          find_template(uri, nil, :partial, options)
        template.updated_at
      end

      # @see Base#key
      def key(uri, options)
        [self.class.name + ":" + uri.split('/')[0...-1].join('/'),
          uri.split('/')[-1] + "." + options[:syntax].to_s]
      end

      # @see Base#to_s
      def to_s
        "(Rails importer)"
      end

      private

      def find_(uri, prefix, options)
        prepare_template(
          find_template(uri, prefix, !:partial, options) ||
            find_template(uri, prefix, :partial, options),
          options)
      end

      def find_template(uri, prefix, partial, options)
        return options[:_rails_lookup_context].
          find_all(uri, prefix, partial).
          find {|t| t.handler.is_a?(Sass::Plugin::TemplateHandler)}
      end

      def prepare_template(template, options)
        return unless template
        options[:syntax] = template.handler.syntax
        options[:filename] = template.virtual_path
        options[:_rails_filename] = template.identifier
        options[:importer] = self
        Sass::Engine.new(template.source, options)
      end
    end
  end
end
