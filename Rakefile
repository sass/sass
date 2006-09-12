require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'
$:.unshift File.join(File.dirname(__FILE__), "..", "lib")

desc 'Default: run unit tests.'
task :default => :test

desc 'Test the haml plugin.'
Rake::TestTask.new(:test) do |t|
  t.libs << 'lib'
  t.pattern = 'test/**/*_test.rb'
  t.verbose = true
end

desc 'Generate documentation for the haml plugin.'
Rake::RDocTask.new(:rdoc) do |rdoc|
  rdoc.rdoc_dir = 'rdoc'
  rdoc.title    = 'Haml'
  rdoc.options << '--line-numbers' << '--inline-source'
  rdoc.rdoc_files.include('README')
  rdoc.rdoc_files.include('lib/**/*.rb')
end

task :rcov do
  `rcov test/*.rb`
end
