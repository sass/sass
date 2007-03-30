require 'haml'
require 'haml/template'
require 'sass'
require 'sass/plugin'
require 'templated_mailer/templated_mailer'

ActionView::Base.register_template_handler('haml', Haml::Template)
ActionMailer::Base.register_template_extension('haml')
Sass::Plugin.update_stylesheets
