dir = File.dirname(__FILE__)
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'haml/version'

# The module that contains everything Haml-related:
#
# * {Haml::Engine} is the class used to render Haml within Ruby code.
# * {Haml::Helpers} contains Ruby helpers available within Haml templates.
# * {Haml::Template} interfaces with web frameworks (Rails in particular).
# * {Haml::Error} is raised when Haml encounters an error.
# * {Haml::HTML} handles conversion of HTML to Haml.
#
# Also see the {file:HAML_REFERENCE.md full Haml reference}.
module Haml
  extend Haml::Version

  # A string representing the version of Haml.
  # A more fine-grained representation is available from Haml.version.
  # @api public
  VERSION = version[:string] unless defined?(Haml::VERSION)

  # Initializes Haml for Rails.
  #
  # This method is called by `init.rb`,
  # which is run by Rails on startup.
  # We use it rather than putting stuff straight into `init.rb`
  # so we can change the initialization behavior
  # without modifying the file itself.
  #
  # @param binding [Binding] The context of the `init.rb` file.
  #   This isn't actually used;
  #   it's just passed in in case it needs to be used in the future
  def self.init_rails(binding)
    # No &method here for Rails 2.1 compatibility
    %w[haml/template sass sass/plugin].each {|f| require f}
  end
end

require 'haml/util'
unless $0 =~ /sass(-convert)?$/
  require 'haml/engine'
  require 'haml/railtie'
end
