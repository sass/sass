# ----- Utility Functions -----

def scope(path)
  File.join(File.dirname(__FILE__), path)
end

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
  test_files = FileList[scope('test/**/*_test.rb')]
  test_files.exclude(scope('test/rails/*'))
  test_files.exclude(scope('test/plugins/*'))
  test_files.exclude(scope('test/haml/spec/*'))
  t.test_files = test_files
  t.verbose = true
end
Rake::Task[:test].send(:add_comment, <<END)
To run with an alternate version of Rails, make test/rails a symlink to that version.
END

# ----- Packaging -----

# Don't use Rake::GemPackageTast because we want prerequisites to run
# before we load the gemspec.
desc "Build all the packages."
task :package => [:revision_file, :submodules, :permissions] do
  load scope('haml.gemspec')
  Gem::Builder.new(HAML_GEMSPEC).build
  pkg = "#{HAML_GEMSPEC.name}-#{HAML_GEMSPEC.version}"
  mkdir_p "pkg"
  verbose(true) {mv "#{pkg}.gem", "pkg/#{pkg}.gem"}

  sh %{rm -f pkg/#{pkg}.tar.gz}
  verbose(false) {HAML_GEMSPEC.files.each {|f| sh %{tar rf pkg/#{pkg}.tar #{f}}}}
  sh %{gzip pkg/#{pkg}.tar}
end

task :permissions do
  sh %{chmod -R a+rx bin}
  sh %{chmod -R a+r .}
  require 'shellwords'
  Dir.glob('test/**/*_test.rb') do |file|
    next if file =~ %r{^test/haml/spec/}
    sh %{chmod a+rx #{file}}
  end
end

task :revision_file do
  require 'lib/haml'

  release = Rake.application.top_level_tasks.include?('release') || File.exist?(scope('EDGE_GEM_VERSION'))
  if Haml.version[:rev] && !release
    File.open(scope('REVISION'), 'w') { |f| f.puts Haml.version[:rev] }
  elsif release
    File.open(scope('REVISION'), 'w') { |f| f.puts "(release)" }
  else
    File.open(scope('REVISION'), 'w') { |f| f.puts "(unknown)" }
  end
end

# We also need to get rid of this file after packaging.
at_exit { File.delete(scope('REVISION')) rescue nil }

desc "Install Haml as a gem. Use SUDO=1 to install with sudo."
task :install => [:package] do
  gem  = RUBY_PLATFORM =~ /java/  ? 'jgem' : 'gem' 
  sh %{#{'sudo ' if ENV["SUDO"]}#{gem} install --no-ri pkg/haml-#{File.read(scope('VERSION')).strip}}
end

desc "Release a new Haml package to Rubyforge."
task :release => [:check_release, :package] do
  name = File.read(scope("VERSION_NAME")).strip
  version = File.read(scope("VERSION")).strip
  sh %{rubyforge add_release haml haml "#{name} (v#{version})" pkg/haml-#{version}.gem}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.gz}
  sh %{gem push pkg/haml-#{version}.gem}
end

# Ensures that the version have been updated for a new release.
task :check_release do
  version = File.read(scope("VERSION")).strip
  raise "There have been changes since current version (#{version})" if changed_since?(version)
  raise "VERSION_NAME must not be 'Bleeding Edge'" if File.read(scope("VERSION_NAME")) == "Bleeding Edge"
end

# Reads a password from the command line.
#
# @param name [String] The prompt to use to read the password
def read_password(prompt)
  require 'readline'
  system "stty -echo"
  Readline.readline("#{prompt}: ").strip
ensure
  system "stty echo"
  puts
end

# Returns whether or not the repository, or specific files,
# has/have changed since a given revision.
#
# @param rev [String] The revision to check against
# @param files [Array<String>] The files to check.
#   If this is empty, checks the entire repository
def changed_since?(rev, *files)
  IO.popen("git diff --exit-code #{rev} #{files.join(' ')}") {}
  return !$?.success?
end

# Returns whether or not the given Emacs mode file (haml or sass)
# has changed since the given version.
#
# @param mode [String, Symbol] The name of the mode
# @param version [String] The version number
# @return [String, nil] The version number if the version has changed
def mode_unchanged?(mode, version)
  mode_version = File.read(scope("extra/#{mode}-mode.el")).scan(/^;; Version: (.*)$/).first.first
  return false if mode_version == version
  return mode_version unless changed_since?(mode_version, "extra/#{mode}-mode.el")
  raise "#{mode}-mode.el version is #{version.inspect}, but it has changed as of #{version.inspect}"
  return false
end

task :submodules do
  if File.exist?(File.dirname(__FILE__) + "/.git")
    sh %{git submodule sync}
    sh %{git submodule update --init}
  elsif !File.exist?(File.dirname(__FILE__) + "/vendor/fssm/lib")
    warn <<WARN
WARNING: vendor/fssm doesn't exist, and this isn't a git repository so
I can't get it automatically!
WARN
  end
end

task :release_edge do
  ensure_git_cleanup do
    puts "#{'=' * 50} Running rake release_edge"

    sh %{git checkout edge-gem}
    sh %{git reset --hard origin/edge-gem}
    sh %{git merge origin/master}

    # Get the current master branch version
    version = File.read(scope('VERSION')).strip.split('.')
    pr = version[3]
    version = version.map {|n| n.to_i}
    unless pr || (version[1] % 2 == 1 && version[2] == 0)
      raise "#{version.join('.')} is not a development version" 
    end

    # Bump the edge gem version
    edge_version = File.read(scope('EDGE_GEM_VERSION')).strip.split('.').map {|n| n.to_i}
    if !pr && (edge_version[0..1] != version[0..1])
      # A new master branch version was released, reset the edge gem version
      edge_version[0..1] = version[0..1]
      edge_version[2] = 0
    else
      # Just bump the teeny version
      edge_version[2] += 1
    end
    edge_version = edge_version.join('.')
    File.open(scope('EDGE_GEM_VERSION'), 'w') {|f| f.puts(edge_version)}
    sh %{git commit -m "Bump edge gem version to #{edge_version}." EDGE_GEM_VERSION}
    sh %{git push origin edge-gem}

    # Package the edge gem with the proper version
    File.open(scope('VERSION'), 'w') {|f| f.puts(edge_version)}
    sh %{rake package}
    sh %{git checkout VERSION}

    sh %{rubyforge add_release haml haml-edge "Bleeding Edge (v#{edge_version})" pkg/haml-edge-#{edge_version}.gem}
    sh %{gem push pkg/haml-edge-#{edge_version}.gem}
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

  namespace :doc do
    task :sass do
      require scope('lib/sass')
      Dir[scope("yard/default/**/*.sass")].each do |sass|
        File.open(sass.gsub(/sass$/, 'css'), 'w') do |f|
          f.write(Sass::Engine.new(File.read(sass)).render)
        end
      end
    end

    desc "List all undocumented methods and classes."
    task :undocumented do
      opts = ENV["YARD_OPTS"] || ""
      ENV["YARD_OPTS"] = opts.dup + <<OPTS
 --list --query "
  object.docstring.blank? &&
  !(object.type == :method && object.is_alias?)"
OPTS
      Rake::Task['yard'].execute
    end
  end

  YARD::Rake::YardocTask.new do |t|
    t.files = FileList.new(scope('lib/**/*.rb')) do |list|
      list.exclude('lib/haml/template/patch.rb')
      list.exclude('lib/haml/template/plugin.rb')
      list.exclude('lib/haml/railtie.rb')
      list.exclude('lib/haml/helpers/action_view_mods.rb')
      list.exclude('lib/haml/helpers/xss_mods.rb')
      list.exclude('lib/sass/plugin/merb.rb')
      list.exclude('lib/sass/plugin/rails.rb')
      list.exclude('lib/sass/less.rb')
    end.to_a
    t.options << '--incremental' if Rake.application.top_level_tasks.include?('redoc')
    t.options += FileList.new(scope('yard/*.rb')).to_a.map {|f| ['-e', f]}.flatten
    files = FileList.new(scope('doc-src/*')).to_a.sort_by {|s| s.size} + %w[MIT-LICENSE VERSION]
    t.options << '--files' << files.join(',')
    t.options << '--template-path' << scope('yard')
    t.options << '--title' << ENV["YARD_TITLE"] if ENV["YARD_TITLE"]

    t.before = lambda do
      if ENV["YARD_OPTS"]
        require 'shellwords'
        t.options.concat(Shellwords.shellwords(ENV["YARD_OPTS"]))
      end
    end
  end
  Rake::Task['yard'].prerequisites.insert(0, 'doc:sass')
  Rake::Task['yard'].instance_variable_set('@comment', nil)

  desc "Generate Documentation"
  task :doc => :yard
  task :redoc => :yard
rescue LoadError
  desc "Generate Documentation"
  task :doc => :rdoc
  task :yard => :rdoc
end

task :pages do
  ensure_git_cleanup do
    puts "#{'=' * 50} Running rake pages PROJ=#{ENV["PROJ"].inspect}"
    raise 'No ENV["PROJ"]!' unless proj = ENV["PROJ"]
    sh %{git checkout #{proj}-pages}
    sh %{git reset --hard origin/#{proj}-pages}

    Dir.chdir("/var/www/#{proj}-pages") do
      sh %{git fetch origin}

      sh %{git checkout stable}
      sh %{git reset --hard origin/stable}

      sh %{git checkout #{proj}-pages}
      sh %{git reset --hard origin/#{proj}-pages}
      sh %{rake build --trace}
      sh %{mkdir -p tmp}
      sh %{touch tmp/restart.txt}
    end
  end
end

# ----- Coverage -----

begin
  require 'rcov/rcovtask'

  Rcov::RcovTask.new do |t|
    t.test_files = FileList[scope('test/**/*_test.rb')]
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

      file = File.read(scope("test/sass/templates/#{file || 'complex'}.sass"))
      result = RubyProf.profile { times.times { Sass::Engine.new(file).render } }
    else
      require 'lib/haml'

      file = File.read(scope("test/haml/templates/#{file || 'standard'}.haml"))
      obj = Object.new
      Haml::Engine.new(file).def_method(obj, :render)
      result = RubyProf.profile { times.times { obj.render } }
    end

    RubyProf.const_get("#{(ENV['OUTPUT'] || 'Flat').capitalize}Printer").new(result).print 
  end
rescue LoadError; end

# ----- Testing Multiple Rails Versions -----

rails_versions = [
  "v2.3.5",
  "v2.2.3",
  "v2.1.2",
]
rails_versions << "v2.0.5" if RUBY_VERSION =~ /^1\.8/

def test_rails_version(version)
  Dir.chdir "test/rails" do
    sh %{git checkout #{version}}
  end
  puts "Testing Rails #{version}"
  Rake::Task['test'].reenable
  Rake::Task['test'].execute
end

namespace :test do
  desc "Test all supported versions of rails. This takes a while."
  task :rails_compatibility do
    sh %{rm -rf test/rails}
    puts "Checking out rails. Please wait."
    sh %{git clone git://github.com/rails/rails.git test/rails}
    begin
      rails_versions.each {|version| test_rails_version version}

      puts "Checking out rails_xss. Please wait."
      sh %{git clone git://github.com/NZKoz/rails_xss.git test/plugins/rails_xss}
      test_rails_version(rails_versions.find {|s| s =~ /^v2\.3/})
    ensure
      `rm -rf test/rails`
      `rm -rf test/plugins`
    end
  end
end

# ----- Handling Updates -----

def email_on_error
  yield
rescue Exception => e
  IO.popen("sendmail nex342@gmail.com", "w") do |sm|
    sm << "From: nex3@nex-3.com\n" <<
      "To: nex342@gmail.com\n" <<
      "Subject: Exception when running rake #{Rake.application.top_level_tasks.join(', ')}\n" <<
      e.message << "\n\n" <<
      e.backtrace.join("\n")
  end
ensure
  raise e if e
end

def ensure_git_cleanup
  email_on_error {yield}
ensure
  sh %{git reset --hard HEAD}
  sh %{git clean -xdf}
  sh %{git checkout master}
end

task :handle_update do
  email_on_error do
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
end
