require 'rubygems'
require 'rake'
require 'rake/testtask'
require 'rake/rdoctask'

volatile_requires = ['rcov/rcovtask']
not_loaded = []
volatile_requires.each do |file|
  begin
    require file
  rescue LoadError
    not_loaded.push file
  end
end

# ----- Default: Testing ------

desc 'Default: run unit tests.'
task :default => :test

desc 'Test the HAML plugin'
Rake::TestTask.new(:test) do |t|
  t.libs << 'lib'
  t.pattern = 'test/**/*_test.rb'
  t.verbose = true
end

# ----- Benchmarking -----

temp_desc = <<END
Benchmark HAML against ERb.
  TIMES=n sets the number of runs. Defaults to 100.
END
desc temp_desc.chomp
task :benchmark do
  require 'test/benchmark'

  puts '-'*51, "Benchmark: HAML vs. ERb", '-'*51
  puts "Running benchmark #{ENV['TIMES']} times..." if ENV['TIMES']
  args = []
  args.push ENV['TIMES'].to_i if ENV['TIMES']
  benchmarker = Haml::Benchmarker.new
  puts benchmarker.benchmark(*args)
  puts '-'*51
end

# ----- Documentation -----

rdoc_task = Proc.new do |rdoc|
  rdoc.title    = 'Haml'
  rdoc.options << '--line-numbers' << '--inline-source'
  rdoc.rdoc_files.include('REFERENCE')
  rdoc.rdoc_files.include('lib/**/*.rb')
  rdoc.rdoc_files.exclude('lib/haml/buffer.rb')
end

Rake::RDocTask.new do |rdoc|
  rdoc_task.call(rdoc)
  rdoc.rdoc_dir = 'rdoc'
end

Rake::RDocTask.new(:rdoc_devel) do |rdoc|
  rdoc_task.call(rdoc)
  rdoc.rdoc_dir = 'rdoc_devel'
  rdoc.options << '--all'
  rdoc.rdoc_files.include('test/*.rb')
  rdoc.rdoc_files = Rake::FileList.new(*rdoc.rdoc_files.to_a)
  rdoc.rdoc_files.include('lib/haml/buffer.rb')
end

# ----- Coverage -----

unless not_loaded.include? 'rcov/rcovtask'
  Rcov::RcovTask.new do |t|
    t.libs << "test"
    t.test_files = FileList['test/*_test.rb']
    if ENV['NON_NATIVE']
      t.rcov_opts << "--no-rcovrt"
    end
    t.verbose = true
  end
end

# ----- Profiling -----

temp_desc = <<END
Run a profile of HAML.
  TIMES=n sets the number of runs. Defaults to 100.
  FILE=n sets the file to profile. Defaults to 'standard'.
END
desc temp_desc.chomp
task :profile do
  require 'test/profile'
  
  puts '-'*51, "Profiling HAML::Template", '-'*51
  
  args = []
  args.push ENV['TIMES'].to_i if ENV['TIMES']
  args.push ENV['FILE'] if ENV['FILE']
  
  profiler = Haml::Profiler.new
  res = profiler.profile(*args)
  puts res
  
  puts '-'*51
end
