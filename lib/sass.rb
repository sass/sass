dir = File.dirname(__FILE__)
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'haml/version'

# The module that contains everything Sass-related:
#
# * {Sass::Engine} is the class used to render Sass within Ruby code.
# * {Sass::Plugin} is interfaces with web frameworks (Rails and Merb in particular).
# * {Sass::SyntaxError} is raised when Sass encounters an error.
# * {Sass::CSS} handles conversion of CSS to Sass.
#
# Also see the {file:SASS_REFERENCE.md full Sass reference}.
module Sass
  extend Haml::Version

  # A string representing the version of Sass.
  # A more fine-grained representation is available from {Sass.version}.
  VERSION = version[:string] unless defined?(Sass::VERSION)
end

require 'haml/util'
require 'sass/engine'
require 'sass/plugin' if defined?(Merb::Plugins)
