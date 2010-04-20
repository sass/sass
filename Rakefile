require 'rubygems'
require 'rake'

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

require 'rake/gempackagetask'
load scope('haml.gemspec')

Rake::GemPackageTask.new(HAML_GEMSPEC) do |pkg|
  if Rake.application.top_level_tasks.include?('release')
    pkg.need_tar_gz  = true
    pkg.need_tar_bz2 = true
    pkg.need_zip     = true
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
Rake::Task[:package].prerequisites.insert(0, :revision_file)

# We also need to get rid of this file after packaging.
at_exit { File.delete(scope('REVISION')) rescue nil }

desc "Install Haml as a gem."
task :install => [:package] do
  sudo = RUBY_PLATFORM =~ /win32|mingw/ ? '' : 'sudo'
  gem  = RUBY_PLATFORM =~ /java/  ? 'jgem' : 'gem' 
  sh %{#{sudo} #{gem} install --no-ri pkg/haml-#{File.read(scope('VERSION')).strip}}
end

desc "Release a new Haml package to Rubyforge."
task :release => [:check_release, :release_elpa, :package] do
  name = File.read(scope("VERSION_NAME")).strip
  version = File.read(scope("VERSION")).strip
  sh %{rubyforge add_release haml haml "#{name} (v#{version})" pkg/haml-#{version}.gem}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.gz}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.tar.bz2}
  sh %{rubyforge add_file    haml haml "#{name} (v#{version})" pkg/haml-#{version}.zip}
  sh %{gem push pkg/haml-#{version}.gem}
end

# Releases haml-mode.el and sass-mode.el to ELPA.
task :release_elpa do
  require 'tlsmail'
  require 'time'
  require scope('lib/haml')

  version = Haml.version[:number]

  haml_unchanged = mode_unchanged?(:haml, version)
  sass_unchanged = mode_unchanged?(:sass, version)
  next if haml_unchanged && sass_unchanged
  raise "haml-mode.el and sass-mode.el are out of sync." if haml_unchanged ^ sass_unchanged

  if sass_unchanged && File.read(scope("extra/sass-mode.el")).
      include?(";; Package-Requires: ((haml-mode #{sass_unchanged.inspect}))")
    raise "sass-mode.el doesn't require the same version of haml-mode."
  end

  from = `git config user.email`.strip
  raise "Don't know how to send emails except via Gmail" unless from =~ /@gmail.com$/

  to = "elpa@tromey.com"
  Net::SMTP.enable_tls(OpenSSL::SSL::VERIFY_NONE)
  Net::SMTP.start('smtp.gmail.com', 587, 'gmail.com', from, read_password("GMail Password"), :login) do |smtp|
    smtp.send_message(<<CONTENT, from, to)
From: Nathan Weizenbaum <#{from}>
To: #{to}
Subject: Submitting haml-mode and sass-mode #{version}
Date: #{Time.now.rfc2822}

haml-mode and sass-mode #{version} are packaged and ready to be included in ELPA.
They can be downloaded from:

  http://github.com/nex3/haml/raw/#{Haml.version[:rev]}/extra/haml-mode.el
  http://github.com/nex3/haml/raw/#{Haml.version[:rev]}/extra/sass-mode.el
CONTENT
  end
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

task :release_edge do
  ensure_git_cleanup do
    puts "#{'=' * 50} Running rake release_edge"

    sh %{git checkout edge-gem}
    sh %{git reset --hard origin/edge-gem}
    sh %{git merge origin/master}

    # Get the current master branch version
    version = File.read(scope('VERSION')).strip.split('.').map {|n| n.to_i}
    unless version[1] % 2 == 1 && version[2] == 0
      raise "#{version.join('.')} is not a development version" 
    end

    # Bump the edge gem version
    edge_version = File.read(scope('EDGE_GEM_VERSION')).strip.split('.').map {|n| n.to_i}
    if edge_version[0..1] != version[0..1]
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
      list.exclude('lib/haml/template/*.rb')
      list.exclude('lib/haml/railtie.rb')
      list.exclude('lib/haml/helpers/action_view_mods.rb')
      list.exclude('lib/haml/helpers/xss_mods.rb')
      list.exclude('lib/sass/plugin/merb.rb')
      list.exclude('lib/sass/plugin/rails.rb')
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

    sh %{rake build --trace}
    sh %{rsync -av --delete site/ /var/www/#{proj}-pages}
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
    `git checkout #{version}`
  end
  puts "Testing Rails #{version}"
  Rake::Task['test'].reenable
  Rake::Task['test'].execute
end

namespace :test do
  desc "Test all supported versions of rails. This takes a while."
  task :rails_compatibility do
    `rm -rf test/rails`
    puts "Checking out rails. Please wait."
    system("git clone git://github.com/rails/rails.git test/rails") rescue nil
    begin
      rails_versions.each {|version| test_rails_version version}

      puts "Checking out rails_xss. Please wait."
      system("git clone git://github.com/NZKoz/rails_xss.git test/plugins/rails_xss")
      test_rails_version(rails_versions.find {|s| s =~ /^v2\.3/})
    ensure
      `rm -rf test/rails`
      `rm -rf test/plugins`
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
  unless ENV["REF"] =~ %r{^refs/heads/(master|(?:haml|sass)-pages)$}
    puts "#{'=' * 20} Ignoring rake handle_update REF=#{ENV["REF"].inspect}"
    next
  end
  branch = $1

  puts
  puts
  puts '=' * 150
  puts "Running rake handle_update REF=#{ENV["REF"].inspect}"

  sh %{git checkout master}
  sh %{git fetch origin}
  sh %{git reset --hard origin/master}

  if branch == "master"
    sh %{rake release_edge --trace}
    sh %{rake pages --trace PROJ=haml}
    sh %{rake pages --trace PROJ=sass}
  elsif branch =~ /^(haml|sass)-pages$/
    sh %{rake pages --trace PROJ=#{$1}}
  end

  puts 'Done running handle_update'
  puts '=' * 150
end
