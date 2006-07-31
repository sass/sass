require 'haml'
require 'haml_helpers'

ActionView::Base.register_template_handler("haml", HAML::TemplateEngine)