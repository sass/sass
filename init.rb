require 'haml/template'
require 'sass/plugin'

ActionView::Base.register_template_handler('haml', Haml::Template)