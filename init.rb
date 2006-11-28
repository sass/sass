require 'haml/template'
require 'sass/sass_helper'

ActionView::Base.register_template_handler('haml', Haml::Template)
ActionView::Base.class_eval do
  include Sass::SassHelper
end