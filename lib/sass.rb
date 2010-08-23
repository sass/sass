dir = File.dirname(__FILE__)
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'sass/version'

# The module that contains everything Sass-related:
#
# * {Sass::Engine} is the class used to render Sass/SCSS within Ruby code.
# * {Sass::Plugin} is interfaces with web frameworks (Rails and Merb in particular).
# * {Sass::SyntaxError} is raised when Sass encounters an error.
# * {Sass::CSS} handles conversion of CSS to Sass.
#
# Also see the {file:SASS_REFERENCE.md full Sass reference}.
module Sass
  extend Sass::Version

  # A string representing the version of Sass.
  # A more fine-grained representation is available from {Sass::Version#version Sass.version}.
  # @api public
  VERSION = version[:string] unless defined?(Sass::VERSION)

  # Initializes Sass for Rails.
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
    %w[sass sass/plugin].each {|f| require f}
  end
end

require 'sass/util'

dir = Sass::Util.scope("vendor/fssm/lib")
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'sass/engine'
require 'sass/plugin' if defined?(Merb::Plugins)
