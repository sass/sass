# allows testing with edge Rails by creating a test/rails symlink
vendored_rails = File.dirname(__FILE__) + '/../rails'

if File.exists? vendored_rails
  puts "[ using vendored Rails ]"
  $:.unshift vendored_rails + '/activesupport/lib'
  $:.unshift vendored_rails + '/actionpack/lib'
else
  require 'rubygems'
end
require 'action_controller'
require 'action_view'

require 'test/unit'
require File.dirname(__FILE__) + '/../../lib/haml'
