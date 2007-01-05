require 'rubygems'
require 'rake'

volatile_requires = ['rcov/rcovtask']
not_loaded = []
volatile_requires.each do |file|
  begin
    require file
  rescue LoadError
    not_loaded.push file
  end
end

# For some crazy reason,
# some Rake tasks interfere with others
# (specifically, benchmarking).
# Thus, it's advantageous to only show
# the task currently being used.
def is_task?(*tasks)
  ARGV[0].nil? || tasks.include?(ARGV[0])
end

# ----- Default: Testing ------

desc 'Default: run unit tests.'
task :default => :test

if is_task?('test', 'default')
  require 'rake/testtask'

  desc 'Test the Haml plugin'
  Rake::TestTask.new(:test) do |t|
    t.libs << 'lib'
    t.pattern = 'test/**/*_test.rb'
    t.verbose = true
  end
end

# ----- Packaging -----

if is_task?('package', 'repackage', 'clobber_package')
  require 'rake/gempackagetask'
  
  spec = Gem::Specification.new do |spec|
    spec.name = 'haml'
    spec.summary = 'An elegant, structured XHTML/XML templating engine.'
    spec.version = File.read('VERSION').strip
    spec.author = 'Hampton Catlin'
    spec.email = 'haml@googlegroups.com'
    spec.description = <<-END
      Haml (HTML Abstraction Markup Language) is a layer on top of XHTML or XML
      that's designed to express the structure of XHTML or XML documents
      in a non-repetitive, elegant, easy way,
      using indentation rather than closing tags
      and allowing Ruby to be embedded with ease.
      It was originally envisioned as a plugin for Ruby on Rails,
      but it can function as a stand-alone templating engine.
    END
    
    readmes = FileList.new('*') { |list| list.exclude(/[a-z]/) }.to_a
    spec.executables = ['haml']
    spec.files = FileList['lib/**/*', 'bin/*', 'test/**/*', 'Rakefile'].to_a + readmes
    spec.homepage = 'http://haml.hamptoncatlin.com/'
    spec.has_rdoc = true
    spec.extra_rdoc_files = readmes
    spec.rdoc_options += [
      '--title', 'Haml',
      '--main', 'REFERENCE',
      '--exclude', 'lib/haml/buffer.rb',
      '--line-numbers',
      '--inline-source'
    ]
    spec.test_files = FileList['test/**/*_test.rb'].to_a
  end
  
  Rake::GemPackageTask.new(spec) do |pkg|
    pkg.need_zip     = true
    pkg.need_tar_gz  = true
    pkg.need_tar_bz2 = true
  end
end

# ----- Benchmarking -----

if is_task?('benchmark')
  temp_desc = <<END
Benchmark HAML against ERb.
  TIMES=n sets the number of runs. Defaults to 100.
END
  
  desc temp_desc.chomp
  task :benchmark do
    require 'test/benchmark'

    puts '-'*51, "Benchmark: Haml vs. ERb", '-'*51
    puts "Running benchmark #{ENV['TIMES']} times..." if ENV['TIMES']
    times = ENV['TIMES'].to_i if ENV['TIMES']
    benchmarker = Haml::Benchmarker.new
    puts benchmarker.benchmark(times || 100)
    puts '-'*51
  end
end

# ----- Documentation -----

if is_task?('rdoc', 'rerdoc', 'clobber_rdoc', 'rdoc_devel', 'rerdoc_devel', 'clobber_rdoc_devel')
  require 'rake/rdoctask'
  
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
end

# ----- Coverage -----

if is_task?('rcov', 'clobber_rcov')
  unless not_loaded.include? 'rcov/rcovtask'
    Rcov::RcovTask.new do |t|
      t.libs << "test"
      t.test_files = FileList['test/**/*_test.rb']
      if ENV['NON_NATIVE']
        t.rcov_opts << "--no-rcovrt"
      end
      t.verbose = true
    end
  end
end

# ----- Profiling -----

if is_task?('profile')
  temp_desc = <<END
Run a profile of HAML.
  TIMES=n sets the number of runs. Defaults to 100.
  FILE=n sets the file to profile. Defaults to 'standard'.
END
  desc temp_desc.chomp
  task :profile do
    require 'test/profile'
    
    puts '-'*51, "Profiling Haml::Template", '-'*51
    
    args = []
    args.push ENV['TIMES'].to_i if ENV['TIMES']
    args.push ENV['FILE'] if ENV['FILE']
    
    profiler = Haml::Profiler.new
    res = profiler.profile(*args)
    puts res
    
    puts '-'*51
  end
end
