require 'haml'
require 'haml/template'
require 'sass'
require 'sass/plugin'

ActionView::Base.register_template_handler('haml', Haml::Template)
Sass::Plugin.update_stylesheets
