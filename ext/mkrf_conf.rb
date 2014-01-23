require 'rubygems'
require 'rubygems/command.rb'
require 'rubygems/dependency_installer.rb' 

# This script installs the correct version of listen. Listen versions
# beyond 1.1 don't support Ruby 1.8, any RubyGems isn't clever enough
# to install the most recent version that works, so we have to do it
# manually.

Gem::Command.build_args = ARGV
inst = Gem::DependencyInstaller.new
if RUBY_VERSION < "1.9"
  puts "Installing listen ~> 1.1.0"
  inst.install "listen", "~> 1.1.0"
else
  puts "Installing listen >= 1.1.0, < 2.5"
  inst.install "listen", Gem::Requirement.new(">= 1.1.0", "< 2.5")
end

# Create a dummy rakefile to indicate success.
f = File.open(File.join(File.dirname(__FILE__), "Rakefile"), "w")
f.write("task :default\n")
f.close
