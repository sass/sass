require File.dirname(__FILE__) + "/../lib/sass"

class YARD::Generators::FullDocGenerator
  protected

  def generate_assets_with_haml
    generate_assets_without_haml

    if format == :html && serializer
      template_file = find_template template_path(css_file)
      haml_style = Sass::Engine.new(File.read(
          File.dirname(__FILE__) + "/haml-style.sass")).render
      serializer.serialize(css_file, File.read(template_file) + haml_style)
    end

    true
  end
  alias_method :generate_assets_without_haml, :generate_assets
  alias_method :generate_assets, :generate_assets_with_haml
end
