require 'rubygems'
require 'rake'
require 'date'

$: << File.dirname(__FILE__)

if defined?(Encoding)
  Encoding.default_external = 'UTF-8'
  Encoding.default_internal = 'UTF-8'
end

def staticmatic(command)
  require '.haml/lib/haml'
  require '.sass/lib/sass'
  require 'staticmatic'
  configuration = StaticMatic::Configuration.new
  eval(File.read("src/configuration.rb"))
  StaticMatic::Base.new(".", configuration).run(command)
end

desc "Build everything."
task :build => [:site, :yardoc]

desc "Use StaticMatic to build the site."
task(:site => [:haml, :sass]) {staticmatic "build"}

desc "Preview the site with StaticMatic."
task(:preview => [:haml, :sass]) {staticmatic "preview"}

desc "Build the YARD documentation."
task :yardoc => :sass do
  require 'fileutils'
  Dir.chdir(".sass") {sh %{rake doc ANALYTICS=UA-535380-8 YARD_TITLE="Sass Documentation"}}
  FileUtils.mkdir_p("site/docs")
  FileUtils.rm_rf("site/docs/yardoc")
  FileUtils.mv(".sass/doc", "site/docs/yardoc")
end

task :haml => ".haml" do
  Dir.chdir(".haml") do
    sh %{git fetch}
    sh %{git checkout origin/stable}
    # Check out the most recent released stable version
    sh %{git checkout #{File.read("VERSION").strip}}
  end
end

task :sass => ".sass" do
  Dir.chdir(".sass") do
    sh %{git fetch}
    sh %{git checkout origin/stable}
    # Check out the most recent released stable version
    sh %{git checkout #{File.read("VERSION").strip}}
  end
end

file ".haml" do
  sh %{git clone git://github.com/nex3/haml.git .haml}
  Dir.chdir(".haml") {sh %{git checkout origin/stable}}
end

file ".sass" do
  sh %{git clone -l -s . .sass}
  Dir.chdir(".sass") {sh %{git checkout origin/stable}}
end

task(:default) {puts "Dummy default task for RunCodeRun"}
