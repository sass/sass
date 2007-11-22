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

    times = Benchmark.bmbm do |b|
      b.report("haml:")   { runs.times { Haml::Engine.new(haml_template).render } }
      b.report("erb:")    { runs.times { ERB.new(erb_template, nil, '-').render } }
      b.report("erubis:") { runs.times { Erubis::Eruby.new(erb_template).result } }
      b.report("mab:")    { runs.times { Markaby::Template.new(markaby_template).render } }
    end
    
    ratio = sprintf("%g", times[0].to_a[5] / times[1].to_a[5])
    puts "Haml/ERB:     " + ratio
 
    ratio = sprintf("%g", times[0].to_a[5] / times[2].to_a[5])
    puts "Haml/Erubis:  " + ratio
   
    ratio = sprintf("%g", times[0].to_a[5] / times[3].to_a[5])
    puts "Haml/Markaby: " + ratio
    
    puts '', '-' * 50, 'Sass on its own', '-' * 50
    sass_template = File.read("#{File.dirname(__FILE__)}/sass/templates/complex.sass")
    
    Benchmark.bmbm do |b|
      b.report("sass:") { runs.times { Sass::Engine.new(sass_template).render } }
    end
  end
end
