require 'haml/engine'
require 'rubygems'
require 'active_support'
require 'action_view'

module Haml
  class Template
    class << self
      @@options = {}

      # Gets various options for Haml. See README for details.
      def options
        @@options
      end

      # Sets various options for Haml. See README for details.
      def options=(value)
        @@options = value
      end
    end
  end
end

# Decide how we want to load Haml into Rails.
# Patching was necessary for versions <= 2.0.1,
# but we can make it a normal handler for higher versions.
if defined?(ActionView::TemplateHandler)
  require 'haml/template/plugin'
else
  require 'haml/template/patch'
end
