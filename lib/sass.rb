dir = File.dirname(__FILE__)
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'haml/version'

# The module that contains everything Sass-related:
#
# * {Sass::Engine} is the class used to render Sass/SCSS within Ruby code.
# * {Sass::Plugin} is interfaces with web frameworks (Rails and Merb in particular).
# * {Sass::SyntaxError} is raised when Sass encounters an error.
# * {Sass::CSS} handles conversion of CSS to Sass.
#
# Also see the {file:SASS_REFERENCE.md full Sass reference}.
module Sass
  extend Haml::Version

  # A string representing the version of Sass.
  # A more fine-grained representation is available from {Haml::Version#version Sass.version}.
  # @api public
  VERSION = version[:string] unless defined?(Sass::VERSION)

  # Compile a Sass or SCSS string to CSS.
  # Defaults to SCSS.
  # @raise [Sass::SyntaxError] if there's any problems
  def self.compile(contents, options = {})
    options[:syntax] ||= :scss
    Engine.new(contents, options).to_css
  end

  # Compile a file to CSS
  # This function has two modes:
  #
  # Compiles `filename` and writes the output to `css_filename`, returning nil.
  #
  #     Sass.compile_file(filename, css_filename, options)
  #
  # Compiles `filename` and returns a string
  #
  #     Sass.compile_file(filename, options)
  #
  # @raise [Sass::SyntaxError] if there's any problems
  def self.compile_file(filename, *args)
    options = args.last.is_a?(Hash) ? args.pop : {}
    css_filename ||= args.shift
    if options[:syntax].nil? && filename =~ /\.(css|sass|scss)$/
      options[:syntax] = $1.to_sym
    end
    options[:filename] = filename
    options[:css_filename] = css_filename
    result = compile(File.read(filename), options)
    if css_filename
      open(css_filename,"w") {|css_file| css_file.write(result) }
      nil
    else
      result
    end
  end

end

require 'haml/util'

dir = Haml::Util.scope("vendor/fssm/lib")
$LOAD_PATH.unshift dir unless $LOAD_PATH.include?(dir)

require 'sass/engine'
require 'sass/plugin' if defined?(Merb::Plugins)
