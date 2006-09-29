require 'haml/engine'
require 'haml/helpers'

ActionView::Base.register_template_handler('haml', Haml::Template)