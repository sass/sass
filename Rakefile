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

# ----- Benchmarking -----

temp_desc = <<END
Benchmark haml against ERb.
  TIMES=n sets the number of runs. Defaults to 100.
END

desc temp_desc.chomp
task :benchmark do
  require 'test/benchmark'

  puts "Running benchmarks #{ENV['TIMES']} times..." if ENV['TIMES']
  times = ENV['TIMES'].to_i if ENV['TIMES']
  Haml.benchmark(times || 100)
  puts '-'*51
end

# Benchmarking gets screwed up if some other tasks have been
# initialized.
unless ARGV[0] == 'benchmark'

  # ----- Default: Testing ------

  desc 'Default: run unit tests.'
  task :default => :test

  require 'rake/testtask'

  Rake::TestTask.new do |t|
    t.libs << 'lib'
    t.pattern = 'test/**/*_test.rb'
    t.verbose = true
  end
  Rake::Task[:test].send(:add_comment, <<END)
To run with an alternate version of Rails, make test/rails a symlink to that version.
END

  # ----- Packaging -----

  require 'rake/gempackagetask'
  require 'lib/haml'
  load    'haml.gemspec'

  Rake::GemPackageTask.new(HAML_GEMSPEC).define

  desc "This is an internal task."
  task :revision_file do
    if Haml.version[:rev] && !Rake.application.top_level_tasks.include?('release')
      File.open('REVISION', 'w') { |f| f.puts Haml.version[:rev] }
    elsif Rake.application.top_level_tasks.include?('release')
      File.open('REVISION', 'w') { |f| f.puts "(release)" }
    else
      File.open('REVISION', 'w') { |f| f.puts "(unknown)" }
    end
  end
  Rake::Task[:package].prerequisites.insert(0, :revision_file)

  # We also need to get rid of this file after packaging.
  Rake::Task[:package].enhance { File.delete('REVISION') if File.exists?('REVISION') }

  task :install => [:package] do
    sudo = RUBY_PLATFORM =~ /win32/ ? '' : 'sudo'
    sh %{#{sudo} gem install --no-ri pkg/haml-#{File.read('VERSION').strip}}
  end

  task :release => [:package] do
    name, version = ENV['NAME'], ENV['VERSION']
    raise "Must supply NAME and VERSION for release task." unless name && version
    sh %{rubyforge login}
    sh %{rubyforge add_release haml haml "#{name} (v#{version})" pkg/haml-#{version}.gem}
    sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.gz}
    sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.bz2}
    sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.zip}
  end

  # ----- Documentation -----

  begin
    require 'hanna/rdoctask'
  rescue LoadError
    require 'rake/rdoctask'
  end

  rdoc_task = Proc.new do |rdoc|
    rdoc.title    = 'Haml/Sass'
    rdoc.options << '--line-numbers' << '--inline-source'
    rdoc.rdoc_files.include('README.rdoc')
    rdoc.rdoc_files.include('lib/**/*.rb')
    rdoc.rdoc_files.exclude('lib/haml/buffer.rb')
    rdoc.rdoc_files.exclude('lib/sass/tree/*')
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

    # Get rid of exclusion rules
    rdoc.rdoc_files = Rake::FileList.new(*rdoc.rdoc_files.to_a)
    rdoc.rdoc_files.include('lib/haml/buffer.rb')
    rdoc.rdoc_files.include('lib/sass/tree/*')
  end

  # ----- Coverage -----

  unless not_loaded.include? 'rcov/rcovtask'
    Rcov::RcovTask.new do |t|
      t.libs << "test"
      t.test_files = FileList['test/**/*_test.rb']
      t.rcov_opts << '-x' << '"^\/"'
      if ENV['NON_NATIVE']
        t.rcov_opts << "--no-rcovrt"
      end
      t.verbose = true
    end
  end

  # ----- Profiling -----

  temp_desc = <<-END
  Run a profile of haml.
    ENGINE=str sets the engine to be profiled (Haml or Sass).
    TIMES=n sets the number of runs. Defaults to 100.
    FILE=n sets the file to profile. Defaults to 'standard'.
  END
  desc temp_desc.chomp
  task :profile do
    require 'test/profile'

    engine = ENV['ENGINE'] && ENV['ENGINE'].downcase == 'sass' ? Sass : Haml

    puts '-'*51, "Profiling #{engine}", '-'*51

    args = []
    args.push ENV['TIMES'].to_i if ENV['TIMES']
    args.push ENV['FILE'] if ENV['FILE']

    profiler = engine::Profiler.new
    res = profiler.profile(*args)
    puts res

    puts '-'*51
  end

end
