require 'haml/template'

ActionView::Base.register_template_handler('haml', Haml::Template)