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

if ENV["RUN_CODE_RUN"] == "true"
  task :default => :"test:rails_compatibility"
else
  task :default => :test
end

require 'rake/testtask'

Rake::TestTask.new do |t|
  t.libs << 'lib'
  test_files = FileList['test/**/*_test.rb']
  test_files.exclude('test/rails/*')
  t.test_files = test_files
  t.verbose = true
end
Rake::Task[:test].send(:add_comment, <<END)
To run with an alternate version of Rails, make test/rails a symlink to that version.
END

# ----- Packaging -----

require 'rake/gempackagetask'
load    'haml.gemspec'

Rake::GemPackageTask.new(HAML_GEMSPEC) do |pkg|
  if Rake.application.top_level_tasks.include?('release')
    pkg.need_tar_gz  = true
    pkg.need_tar_bz2 = true
    pkg.need_zip     = true
  end
end

task :revision_file do
  require 'lib/haml'

  release = Rake.application.top_level_tasks.include?('release') || File.exist?('EDGE_GEM_VERSION')
  if Haml.version[:rev] && !release
    File.open('REVISION', 'w') { |f| f.puts Haml.version[:rev] }
  elsif release
    File.open('REVISION', 'w') { |f| f.puts "(release)" }
  else
    File.open('REVISION', 'w') { |f| f.puts "(unknown)" }
  end
end
Rake::Task[:package].prerequisites.insert(0, :revision_file)

# We also need to get rid of this file after packaging.
at_exit { File.delete('REVISION') rescue nil }

desc "Install Haml as a gem."
task :install => [:package] do
  sudo = RUBY_PLATFORM =~ /win32/ ? '' : 'sudo'
  gem  = RUBY_PLATFORM =~ /java/  ? 'jgem' : 'gem' 
  sh %{#{sudo} #{gem} install --no-ri pkg/haml-#{File.read('VERSION').strip}}
end

desc "Release a new Haml package to Rubyforge. Requires the NAME and VERSION flags."
task :release => [:package] do
  name = File.read("VERSION_NAME").strip
  version = File.read("VERSION").strip
  raise "VERSION_NAME must not be 'Bleeding Edge'" if name == "Bleeding Edge"
  sh %{rubyforge login}
  sh %{rubyforge add_release haml haml "#{name} (v#{version})" pkg/haml-#{version}.gem}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.gz}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.bz2}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.zip}
end

task :release_edge do
  ensure_git_cleanup do
    puts "#{'=' * 50} Running rake release_edge"

    sh %{git checkout edge-gem}
    sh %{git reset --hard origin/edge-gem}
    sh %{git merge origin/master}

    # Get the current master branch version
    version = File.read('VERSION').strip.split('.').map {|n| n.to_i}
    unless version[1] % 2 == 1 && version[2] == 0
      raise "#{version.join('.')} is not a development version" 
    end

    # Bump the edge gem version
    edge_version = File.read('EDGE_GEM_VERSION').strip.split('.').map {|n| n.to_i}
    if edge_version[0..1] != version[0..1]
      # A new master branch version was released, reset the edge gem version
      edge_version[0..1] = version[0..1]
      edge_version[2] = 0
    else
      # Just bump the teeny version
      edge_version[2] += 1
    end
    edge_version = edge_version.join('.')
    File.open('EDGE_GEM_VERSION', 'w') {|f| f.puts(edge_version)}
    sh %{git commit -m "Bump edge gem version to #{edge_version}." EDGE_GEM_VERSION}
    sh %{git push origin edge-gem}

    # Package the edge gem with the proper version
    File.open('VERSION', 'w') {|f| f.puts(edge_version)}
    sh %{rake package}
    sh %{git checkout VERSION}

    sh %{rubyforge login}
    sh %{rubyforge add_release haml haml-edge "Bleeding Edge (v#{edge_version})" pkg/haml-edge-#{edge_version}.gem}
  end
end

task :watch_for_update do
  sh %{ruby extra/update_watch.rb}
end

# ----- Documentation -----

task :rdoc do
  puts '=' * 100, <<END, '=' * 100
Haml uses the YARD documentation system (http://github.com/lsegal/yard).
Install the yard gem and then run "rake doc".
END
end

begin
  require 'yard'

  YARD::Rake::YardocTask.new do |t|
    t.files = FileList.new('lib/**/*.rb') do |list|
      list.exclude('lib/haml/template/*.rb')
      list.exclude('lib/haml/helpers/action_view_mods.rb')
    end.to_a
    t.options << '--use-cache' if Rake.application.top_level_tasks.include?('redoc')
    t.options += FileList.new('yard/*.rb').to_a.map {|f| ['-e', f]}.flatten
    files = FileList.new('doc-src/*').to_a.sort_by {|s| s.size} + %w[MIT-LICENSE VERSION]
    t.options << '--files' << files.join(',')
  end
  Rake::Task['yardoc'].instance_variable_set('@comment', nil)

  desc "Generate Documentation"
  task :doc => :yardoc
  task :redoc => :yardoc
rescue LoadError
  desc "Generate Documentation"
  task :doc => :rdoc
  task :yardoc => :rdoc
end

task :pages do
  ensure_git_cleanup do
    puts "#{'=' * 50} Running rake pages PROJ=#{ENV["PROJ"].inspect}"
    raise 'No ENV["PROJ"]!' unless proj = ENV["PROJ"]
    sh %{git checkout #{proj}-pages}
    sh %{git reset --hard origin/#{proj}-pages}

    sh %{rake build --trace}
    sh %{rsync -av --delete site/ /var/www/#{proj}-pages}
  end
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

begin
  require 'ruby-prof'

  desc <<END
Run a profile of haml.
  ENGINE=str sets the engine to be profiled. Defaults to Haml.
  TIMES=n sets the number of runs. Defaults to 1000.
  FILE=str sets the file to profile.
    Defaults to 'standard' for Haml and 'complex' for Sass.
  OUTPUT=str sets the ruby-prof output format.
    Can be Flat, CallInfo, or Graph. Defaults to Flat. Defaults to Flat.
END
  task :profile do
    engine = (ENV['ENGINE'] || 'haml').downcase
    times  = (ENV['TIMES'] || '1000').to_i
    file   = ENV['FILE']

    if engine == 'sass'
      require 'lib/sass'

      file = File.read("#{File.dirname(__FILE__)}/test/sass/templates/#{file || 'complex'}.sass")
      result = RubyProf.profile { times.times { Sass::Engine.new(file).render } }
    else
      require 'lib/haml'

      file = File.read("#{File.dirname(__FILE__)}/test/haml/templates/#{file || 'standard'}.haml")
      obj = Object.new
      Haml::Engine.new(file).def_method(obj, :render)
      result = RubyProf.profile { times.times { obj.render } }
    end

    RubyProf.const_get("#{(ENV['OUTPUT'] || 'Flat').capitalize}Printer").new(result).print 
  end
rescue LoadError; end

# ----- Testing Multiple Rails Versions -----

rails_versions = [
  "v2.3.0",
  "v2.2.2",
  "v2.1.2",
  "v2.0.5"
]

namespace :test do
  desc "Test all supported versions of rails. This takes a while."
  task :rails_compatibility do
    `rm -rf test/rails`
    puts "Checking out rails. Please wait."
    `git clone git://github.com/rails/rails.git test/rails` rescue nil
    begin
      rails_versions.each do |version|
        Dir.chdir "test/rails" do
          `git checkout #{version}`
        end
        puts "Testing Rails #{version}"
        Rake::Task['test'].reenable
        Rake::Task['test'].execute
      end
    ensure
      `rm -rf test/rails`
    end
  end
end

# ----- Handling Updates -----

def ensure_git_cleanup
  yield
ensure
  sh %{git reset --hard HEAD}
  sh %{git clean -xdf}
  sh %{git checkout master}
end

task :handle_update do
  unless ENV["REF"] =~ %r{^refs/heads/(master|stable|(?:haml|sass)-pages)$}
    puts "#{'=' * 20} Ignoring rake handle_update REF=#{ENV["REF"].inspect}"
    next
  end
  branch = $1

  puts
  puts
  puts '=' * 150
  puts "Running rake handle_update REF=#{ENV["REF"].inspect}"

  sh %{git fetch origin}
  sh %{git checkout stable}
  sh %{git reset --hard origin/stable}
  sh %{git checkout master}
  sh %{git reset --hard origin/master}

  if branch == "master"
    sh %{rake release_edge --trace}
  elsif branch == "stable"
    sh %{rake pages --trace PROJ=haml}
    sh %{rake pages --trace PROJ=sass}
  elsif branch =~ /^(haml|sass)-pages$/
    sh %{rake pages --trace PROJ=#{$1}}
  end

  puts 'Done running handle_update'
  puts '=' * 150
end
