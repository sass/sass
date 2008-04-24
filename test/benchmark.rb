# There's a bizarre error where ActionController tries to load a benchmark file
# and ends up finding this.
# These declarations then cause it to break.
# This only happens when running rcov, though, so we can avoid it.
unless $0 =~ /rcov$/
  require File.dirname(__FILE__) + '/../lib/haml'
  require 'haml'
end

require 'rubygems'
require 'erb'
require 'erubis'
require 'markaby'
require 'benchmark'
require 'stringio'
require 'open-uri'

module Haml
  # Benchmarks Haml against ERB, Erubis, and Markaby and Sass on its own.
  def self.benchmark(runs = 100)
    template_name = 'standard'
    directory = File.dirname(__FILE__) + '/haml'
    haml_template =    File.read("#{directory}/templates/#{template_name}.haml")
    erb_template =   File.read("#{directory}/rhtml/#{template_name}.rhtml")
    markaby_template = File.read("#{directory}/markaby/#{template_name}.mab")

    puts '-'*51, "Haml and Friends: No Caching", '-'*51

    times = Benchmark.bmbm do |b|
      b.report("haml:")   { runs.times { Haml::Engine.new(haml_template).render } }
      b.report("erb:")    { runs.times { ERB.new(erb_template, nil, '-').result } }
      b.report("erubis:") { runs.times { Erubis::Eruby.new(erb_template).result } }
      b.report("mab:")    { runs.times { Markaby::Template.new(markaby_template).render } }
    end

    print_result = proc do |s, n|
      printf "%1$*2$s %3$*4$g",
        "Haml/#{s}:", -13, times[0].to_a[5] / times[n].to_a[5], -17
      printf "%1$*2$s %3$g\n",
        "#{s}/Haml:", -13, times[n].to_a[5] / times[0].to_a[5]
    end
    
    print_result["ERB", 1]
    print_result["Erubis", 2]
    print_result["Markaby", 3]

    puts '', '-' * 50, 'Haml and Friends: Cached', '-' * 50

    obj = Object.new
    Haml::Engine.new(haml_template).def_method(obj, :haml)
    erb = ERB.new(erb_template, nil, '-')
    obj.instance_eval("def erb; #{erb.src}; end")
    Erubis::Eruby.new(erb_template).def_method(obj, :erubis)
    times = Benchmark.bmbm do |b|
      b.report("haml:")   { runs.times { obj.haml      } }
      b.report("erb:")    { runs.times { obj.erb       } }
      b.report("erubis:") { runs.times { obj.erubis    } }
    end    

    print_result["ERB", 1]
    print_result["Erubis", 2]

    puts '', '-' * 50, 'Haml and ERB: Via ActionView', '-' * 50

    require 'active_support'
    require 'action_controller'
    require 'action_view'
    require 'haml/template'

    @base = ActionView::Base.new(File.dirname(__FILE__))
    times = Benchmark.bmbm do |b|
      b.report("haml:") { runs.times { @base.render 'haml/templates/standard' } }
      b.report("erb:")  { runs.times { @base.render 'haml/rhtml/standard'     } }
    end

    print_result["ERB", 1]

    puts '', '-' * 50, 'Haml and ERB: Via ActionView with deep partials', '-' * 50

    @base = ActionView::Base.new(File.dirname(__FILE__))
    times = Benchmark.bmbm do |b|
      b.report("haml:") { runs.times { @base.render 'haml/templates/action_view' } }
      b.report("erb:")  { runs.times { @base.render 'haml/rhtml/action_view'     } }
    end

    print_result["ERB", 1]
    
    puts '', '-' * 50, 'Sass', '-' * 50
    sass_template = File.read("#{File.dirname(__FILE__)}/sass/templates/complex.sass")
    
    Benchmark.bmbm do |b|
      b.report("sass:") { runs.times { Sass::Engine.new(sass_template).render } }
    end
  end
end
