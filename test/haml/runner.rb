require File.dirname(__FILE__) + '/test_helper'
require 'haml/template'
require 'fileutils'

haml_template_engine = Haml::Template.new(ActionView::Base.new)
haml_template_engine.render(File.dirname(__FILE__) + '/templates/standard.haml')

begin
  eval(File.read("template_test.rb"))
rescue StandardError => e
  puts e.backtrace
  puts e.inspect
end
