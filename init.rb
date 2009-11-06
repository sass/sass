begin
  require File.join(File.dirname(__FILE__), 'lib', 'haml') # From here
rescue LoadError
  begin
    require 'haml' # From gem
  rescue LoadError => e
    # gems:install may be run to install Haml with the skeleton plugin
    # but not the gem itself installed.
    # Don't die if this is the case.
    raise e unless defined?(Rake) && Rake.application.top_level_tasks.include?('gems:install')
  end
end

# Load Haml and Sass.
# Haml may be undefined if we're running gems:install.
Haml.init_rails(binding) if defined?(Haml)
