require "sass"
require "sass/plugin"
require File.expand_path(File.dirname(__FILE__) + "/../../sass/mock_importer.rb")
$importer = MockImporter.new
(0..100).each do |i|
  puts "Creating foo_#{i}"
  $importer.add_import("foo_#{i}", <<-SCSS)
    #{ "@import \"foo_#{i-1}\";" if i > 1}
    $foo-#{i}: #{i};
  SCSS
end
