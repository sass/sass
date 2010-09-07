dir = File.dirname(__FILE__)
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

# This is necessary to set so that the Haml code that tries to load Sass
# knows that Sass is indeed loading,
# even if there's some crazy autoload stuff going on.
SASS_BEGUN_TO_LOAD = true unless defined?(SASS_BEGUN_TO_LOAD)

require 'sass/version'

# The module that contains everything Sass-related:
#
# * {Sass::Engine} is the class used to render Sass/SCSS within Ruby code.
# * {Sass::Plugin} is interfaces with web frameworks (Rails and Merb in particular).
# * {Sass::SyntaxError} is raised when Sass encounters an error.
# * {Sass::CSS} handles conversion of CSS to Sass.
#
# Also see the {file:SASS_REFERENCE.md full Sass reference}.
module Sass; end

require 'sass/util'

dir = Sass::Util.scope("vendor/fssm/lib")
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'sass/engine'
require 'sass/plugin' if defined?(Merb::Plugins)
require 'sass/railtie'
