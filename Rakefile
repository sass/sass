require 'rubygems'
require 'rake'

# ----- Benchmarking -----

desc <<END
Benchmark haml against ERb.
  TIMES=n sets the number of runs. Defaults to 1000.
END
task :benchmark do
  sh "ruby test/benchmark.rb #{ENV['TIMES']}"
end

# ----- Default: Testing ------

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

Rake::GemPackageTask.new(HAML_GEMSPEC) do |pkg|
  if Rake.application.top_level_tasks.include?('release')
    pkg.need_tar_gz  = true
    pkg.need_tar_bz2 = true
    pkg.need_zip     = true
  end
end

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

Rake::RDocTask.new do |rdoc|
  rdoc.title    = 'Haml/Sass'
  rdoc.options << '--line-numbers' << '--inline-source'
  rdoc.rdoc_files.include(*FileList.new('*') do |list|
                            list.exclude(/(^|[^.a-z])[a-z]+/)
                            list.exclude('TODO')
                          end.to_a)
  rdoc.rdoc_files.include('lib/**/*.rb')
  rdoc.rdoc_files.exclude('TODO')
  rdoc.rdoc_files.exclude('lib/haml/buffer.rb')
  rdoc.rdoc_files.exclude('lib/sass/tree/*')
  rdoc.rdoc_dir = 'rdoc'
  rdoc.main = 'README.rdoc'
end

# ----- Coverage -----

begin
  require 'rcov/rcovtask'

  Rcov::RcovTask.new do |t|
    t.test_files = FileList['test/**/*_test.rb']
    t.rcov_opts << '-x' << '"^\/"'
    if ENV['NON_NATIVE']
      t.rcov_opts << "--no-rcovrt"
    end
    t.verbose = true
  end
rescue LoadError; end

# ----- Profiling -----

desc <<END
Run a profile of haml.
  ENGINE=str sets the engine to be profiled (Haml or Sass).
  TIMES=n sets the number of runs. Defaults to 100.
  FILE=n sets the file to profile. Defaults to 'standard'.
END
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
