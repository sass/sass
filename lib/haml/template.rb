require 'haml/template/options'
require 'haml/engine'
require 'haml/helpers/action_view_mods'
require 'haml/helpers/action_view_extensions'

if defined?(ActionPack::VERSION::STRING) &&
    ActionPack::VERSION::STRING == "2.3.6"
  raise "Haml does not support Rails 2.3.6. Please upgrade to 2.3.7 or later."
end

module Haml
  # The class that keeps track of the global options for Haml within Rails.
  module Template
    # Enables integration with the Rails 2.2.5+ XSS protection,
    # if it's available and enabled.
    #
    # @return [Boolean] Whether the XSS integration was enabled.
    def try_enabling_xss_integration
      return false unless (ActionView::Base.respond_to?(:xss_safe?) && ActionView::Base.xss_safe?) ||
        # We check for ActiveSupport#on_load here because if we're loading Haml that way, it means:
        # A) we're in Rails 3 so XSS support is always on, and
        # B) we might be in Rails 3 beta 3 where the load order is broken and xss_safe? is undefined
        (defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load))

      Haml::Template.options[:escape_html] = true

      Haml::Util.module_eval {def rails_xss_safe?; true; end}

      require 'haml/helpers/xss_mods'
      Haml::Helpers.send(:include, Haml::Helpers::XssMods)

      Haml::Precompiler.module_eval do
        def precompiled_method_return_value_with_haml_xss
          "::Haml::Util.html_safe(#{precompiled_method_return_value_without_haml_xss})"
        end
        alias_method :precompiled_method_return_value_without_haml_xss, :precompiled_method_return_value
        alias_method :precompiled_method_return_value, :precompiled_method_return_value_with_haml_xss
      end

      true
    end
  end
end

unless Haml::Util.rails_env == "development"
  Haml::Template.options[:ugly] ||= true
end

if Haml::Util.ap_geq_3?
  Haml::Template.options[:format] ||= :html5
end

# Decide how we want to load Haml into Rails.
# Patching was necessary for versions <= 2.0.1,
# but we can make it a normal handler for higher versions.
if defined?(ActionView::TemplateHandler) ||
    (defined?(ActionView::Template) && defined?(ActionView::Template::Handler))
  require 'haml/template/plugin'
else
  require 'haml/template/patch'
end

# Enable XSS integration. Use Rails' after_initialize method
# so that integration will be checked after the rails_xss plugin is loaded
# (for Rails 2.3.* where it's not enabled by default).
#
# If we're running under Rails 3, though, we don't want to use after_intialize,
# since Haml loading has already been deferred via ActiveSupport.on_load.
if defined?(Rails.configuration.after_initialize) &&
    !(defined?(ActiveSupport) && Haml::Util.has?(:public_method, ActiveSupport, :on_load))
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
    Haml::Util.haml_warn <<END
HAML WARNING:
#{rails_init_file} is out of date and couldn't be automatically updated.
Please run `haml --rails #{File.expand_path(Haml::Util.rails_root)}' to update it.
END
  end
end
