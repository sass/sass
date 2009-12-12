require 'haml/engine'

module Haml
  # The class that keeps track of the global options for Haml within Rails.
  module Template
    extend self

    @options = {}
    # The options hash for Haml when used within Rails.
    # See {file:HAML_REFERENCE.md#haml_options the Haml options documentation}.
    #
    # @return [{Symbol => Object}]
    attr_accessor :options

    # Enables integration with the Rails 2.2.5+ XSS protection,
    # if it's available and enabled.
    #
    # @return [Boolean] Whether the XSS integration was enabled.
    def try_enabling_xss_integration
      return false unless ActionView::Base.respond_to?(:xss_safe?) && ActionView::Base.xss_safe?

      Haml::Template.options[:escape_html] = true

      Haml::Util.module_eval {def rails_xss_safe?; true; end}

      require 'haml/helpers/xss_mods'
      Haml::Helpers.send(:include, Haml::Helpers::XssMods)

      Haml::Precompiler.module_eval do
        def precompiled_method_return_value_with_haml_xss
          "(#{precompiled_method_return_value_without_haml_xss}).html_safe!"
        end
        alias_method :precompiled_method_return_value_without_haml_xss, :precompiled_method_return_value
        alias_method :precompiled_method_return_value, :precompiled_method_return_value_with_haml_xss
      end

      true
    end
  end
end

if defined?(RAILS_ENV) && RAILS_ENV == "production"
  Haml::Template.options[:ugly] = true
end

# Decide how we want to load Haml into Rails.
# Patching was necessary for versions <= 2.0.1,
# but we can make it a normal handler for higher versions.
if defined?(ActionView::TemplateHandler) || defined?(ActionView::Template::Handler)
  require 'haml/template/plugin'
else
  require 'haml/template/patch'
end

# Enable XSS integration. Use Rails' after_initialize method if possible
# so that integration will be checked after the rails_xss plugin is loaded
# (for Rails 2.3.* where it's not enabled by default).
if defined?(Rails.configuration.after_initialize)
  Rails.configuration.after_initialize {Haml::Template.try_enabling_xss_integration}
else
  Haml::Template.try_enabling_xss_integration
end

if Haml::Util.rails_root
  # Update init.rb to the current version
  # if it's out of date.
  #
  # We can probably remove this as of v1.9,
  # because the new init file is sufficiently flexible
  # to not need updating.
  rails_init_file = File.join(Haml::Util.rails_root, 'vendor', 'plugins', 'haml', 'init.rb')
  haml_init_file = Haml::Util.scope('init.rb')
  begin
    if File.exists?(rails_init_file)
      require 'fileutils'
      FileUtils.cp(haml_init_file, rails_init_file) unless FileUtils.cmp(rails_init_file, haml_init_file)
    end
  rescue SystemCallError
    warn <<END
HAML WARNING:
#{rails_init_file} is out of date and couldn't be automatically updated.
Please run `haml --rails #{File.expand_path(Haml::Util.rails_root)}' to update it.
END
  end
end
