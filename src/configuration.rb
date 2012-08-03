require 'compass'
require 'cgi'

Compass.configuration do |config|
  # project_path should be the the directory to which the sass directory is relative.
  # I think maybe this should be one more directory up from the configuration file.
  # Please update this if it is or remove this message if it can stay the way it is.
  config.project_path = File.dirname(__FILE__)
  config.sass_dir = File.join('src', 'stylesheets' )
end

# sass_engine_options returns a hash, you can merge it with other options.
configuration.sass_options = Compass.sass_engine_options
configuration.sass_options[:debug_info] = (ARGV[0] == "preview")

module StaticMatic::Helpers
  def local_page
    current_page.gsub(/\.html$/, '').gsub(/\/index$/, '').gsub(/^\//, '')
  end

  def css
    (["#haml"] + local_page.split(File::SEPARATOR)).join(".")
  end

  def h_and_preserve(content = nil, &block)
    return preserve(CGI.escapeHTML(content)) if content
    return h_and_preserve(capture_haml(&block).strip)
  end

  def version_info
    version = "<span class=\"version\">#{Sass.version[:number]}</span>"
    return version unless date = Sass.version[:date]
    version + ", <span class=\"date\">#{date.strftime("%d %B %Y")}</span>"
  end
end
