root = File.expand_path("../..", __FILE__)
File.open(File.expand_path("lib/haml/root.rb", root), "w") do |f|
  f << <<-RUBY
module Haml
  ROOT_DIR = #{root.inspect}
end
  RUBY
end

File.open('Makefile', 'w') { |f| f.puts("install:\n\t$(exit 0)") }
