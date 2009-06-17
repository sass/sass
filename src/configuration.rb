require 'compass'

Compass.configuration do |config|
  # project_path should be the the directory to which the sass directory is relative.
  # I think maybe this should be one more directory up from the configuration file.
  # Please update this if it is or remove this message if it can stay the way it is.
  config.project_path = File.dirname(__FILE__)
  config.sass_dir = File.join('src', 'stylesheets' )
end

# sass_engine_options returns a hash, you can merge it with other options.
configuration.sass_options = Compass.sass_engine_options
