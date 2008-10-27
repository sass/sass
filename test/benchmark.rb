#!/usr/bin/env ruby

times = (ARGV.first || 1000).to_i

if times == 0 # Invalid parameter
  puts <<END
ruby #$0 [times=1000]
  Benchmark Haml against various other templating languages and Sass
  on its own.
END
  exit 1
end

require File.dirname(__FILE__) + '/../lib/haml'
require File.dirname(__FILE__) + '/linked_rails'
%w[sass rubygems erb erubis markaby active_support action_controller
   action_view haml/template].each(&method(:require))

begin
  require 'benchwarmer'
rescue LoadError
  # Since it's not as simple as gem install at the time of writing,
  # we need to direct folks to the benchwarmer gem.
  raise "The Haml benchmarks require the benchwarmer gem, available from http://github.com/wycats/benchwarmer"
end

def view
  unless ActionView::Base.instance_methods.include? 'finder'
    return ActionView::Base.new(File.dirname(__FILE__), vars)
  end

  # Rails >=2.1.0
  base = ActionView::Base.new
  base.finder.append_view_path(File.dirname(__FILE__))
  base
end

Benchmark.warmer(times) do
  columns :haml, :erb, :erubis, :mab
  titles :haml => "Haml", :erb => "ERB", :erubis => "Erubis", :mab => "Markaby"

  template_name = 'standard'
  directory = File.dirname(__FILE__) + '/haml'
  haml_template    = File.read("#{directory}/templates/#{template_name}.haml")
  erb_template     = File.read("#{directory}/rhtml/#{template_name}.rhtml")
  markaby_template = File.read("#{directory}/markaby/#{template_name}.mab")

  report "Uncached" do
    haml   { Haml::Engine.new(haml_template).render }
    erb    { ERB.new(erb_template, nil, '-').result }
    erubis { Erubis::Eruby.new(erb_template).result }
    mab    { Markaby::Template.new(markaby_template).render }
  end

  report "Cached" do
    obj = Object.new

    Haml::Engine.new(haml_template).def_method(obj, :haml)
    Erubis::Eruby.new(erb_template).def_method(obj, :erubis)
    obj.instance_eval("def erb; #{ERB.new(erb_template, nil, '-').src}; end")

    haml   { obj.haml }
    erb    { obj.erb }
    erubis { obj.erubis }
  end

  report "ActionView" do
    @base = view

    # To cache the template
    @base.render 'haml/templates/standard'
    @base.render 'haml/rhtml/standard'

    haml { @base.render 'haml/templates/standard' }
    erb  { @base.render 'haml/rhtml/standard' }
  end

  report "ActionView with deep partials" do
    @base = view

    # To cache the template
    @base.render 'haml/templates/action_view'
    @base.render 'haml/rhtml/action_view'

    haml { @base.render 'haml/templates/action_view' }
    erb  { @base.render 'haml/rhtml/action_view' }
  end
end

Benchmark.warmer(times) do
  sass_template = File.read("#{File.dirname(__FILE__)}/sass/templates/complex.sass")

  report("Sass") { Sass::Engine.new(sass_template).render }
end
