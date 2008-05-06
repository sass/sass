# There's a bizarre error where ActionController tries to load a benchmark file
# and ends up finding this.
# These declarations then cause it to break.
# This only happens when running rcov, though, so we can avoid it.
unless $0 =~ /rcov$/
  require File.dirname(__FILE__) + '/../lib/haml'
  require 'haml'
  require 'sass'
end

require 'rubygems'
require 'erb'
require 'erubis'
require 'markaby'
require 'active_support'
require 'action_controller'
require 'action_view'
require 'haml/template'
begin
  require 'benchwarmer'
rescue LoadError
  # Since it's not as simple as gem install at the time of writing,
  # we need to direct folks to the benchwarmer gem.
  raise "The Haml benchmarks require the benchwarmer gem, available from http://github.com/wycats/benchwarmer"
end

module Haml
  # Benchmarks Haml against ERB, Erubis, and Markaby and Sass on its own.
  def self.benchmark(runs = 100)
    Benchmark.warmer(runs) do
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
        @base = ActionView::Base.new(File.dirname(__FILE__))

        # To cache the template
        @base.render 'haml/templates/standard'
        @base.render 'haml/rhtml/standard'

        haml { @base.render 'haml/templates/standard' }
        erb  { @base.render 'haml/rhtml/standard' }
      end

      report "ActionView with deep partials" do
        @base = ActionView::Base.new(File.dirname(__FILE__))

        # To cache the template
        @base.render 'haml/templates/action_view'
        @base.render 'haml/rhtml/action_view'

        haml { @base.render 'haml/templates/action_view' }
        erb  { @base.render 'haml/rhtml/action_view' }
      end
    end

    Benchmark.warmer(runs) do
      sass_template = File.read("#{File.dirname(__FILE__)}/sass/templates/complex.sass")

      report("Sass") { Sass::Engine.new(sass_template).render }
    end
  end
end
