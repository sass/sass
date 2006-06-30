require 'haml'

ActionView::Base.register_template_handler("haml", HAML::TemplateEngine)