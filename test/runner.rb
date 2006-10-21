require 'rubygems'
require 'active_support'
require 'action_view'
require '../lib/haml/template'
require 'fileutils'

haml_template_engine = Haml::Template.new(ActionView::Base.new)
haml_template_engine.render(File.dirname(__FILE__) + '/templates/standard.haml')

begin
  eval(File.read("template_test.rb"))
rescue StandardError => e
  puts e.backtrace
  puts e.inspect
end
