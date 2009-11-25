# allows testing with edge Rails by creating a test/rails symlink
linked_rails = File.dirname(__FILE__) + '/rails'

if File.exists?(linked_rails) && !$:.include?(linked_rails + '/activesupport/lib')
  puts "[ using linked Rails ]"
  $:.unshift linked_rails + '/activesupport/lib'
  $:.unshift linked_rails + '/actionpack/lib'
end
require 'rubygems'
require 'action_controller'
require 'action_view'

ActionController::Base.logger = Logger.new(nil)

# Load plugins from test/plugins.
# This will only work with very basic plugins,
# since we don't want to load the entirety of Rails.
Dir[File.dirname(__FILE__) + '/plugins/*'].each do |plugin|
  $: << plugin + '/lib'
  Object.new.instance_eval(File.read(plugin + '/init.rb'))
end
