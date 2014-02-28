require 'rubygems'
require 'rubygems/command.rb'
require 'rubygems/dependency_installer.rb'

require '../lib/sass'

# This script installs the correct version of listen. Listen versions
# beyond 1.1 don't support Ruby 1.8, any RubyGems isn't clever enough
# to install the most recent version that works, so we have to do it
# manually.

puts "Ensuring you have the right version of listen installed."

Gem::Command.build_args = ARGV
inst = Gem::DependencyInstaller.new
if Sass::Util.version_geq(RUBY_VERSION, "1.9.3")
  puts "Installing listen >= 1.1.0, < 3.0.0"
  inst.install "listen", Gem::Requirement.new(">= 1.1.0", "< 3.0.0")
else
  puts "Installing listen ~> 1.1.0"
  inst.install "listen", "~> 1.1.0"
end

# Create a dummy rakefile to indicate success.
f = File.open(File.join(File.dirname(__FILE__), "Rakefile"), "w")
f.write("task :default\n")
f.close
