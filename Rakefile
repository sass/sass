require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'
$:.unshift File.join(File.dirname(__FILE__), "..", "lib")

desc 'Default: run unit tests.'
task :default => :test

desc 'Test the HAML plugin'
Rake::TestTask.new(:test) do |t|
  t.libs << 'lib'
  t.pattern = 'test/**/*_test.rb'
  t.verbose = true
end

desc 'Benchmark HAML against ERb. The benchmark routine is run 100. Use TIMES=n to override'
task :benchmark do
  puts '-'*51, "+ Benchmark: HAML vs. ERb", '-'*51
  puts "Running benchmark #{ENV['TIMES']} times..." if ENV['TIMES']
  puts `ruby test/benchmark.rb #{ENV['TIMES']}` 
  puts '-'*51
end

desc 'Generate documentation for the haml plugin. Use DEVEL=1 to generate documentation for private methods as well.'
Rake::RDocTask.new(:rdoc) do |rdoc|
  rdoc.rdoc_dir = 'rdoc'
  rdoc.title    = 'Haml'
  rdoc.options << '--line-numbers' << '--inline-source'
  if ENV['DEVEL']
    rdoc.options << '--all'
  end
  rdoc.rdoc_files.include('README')
  rdoc.rdoc_files.include('lib/**/*.rb')
end

task :rcov do
  `rcov test/*.rb`
end
