require File.dirname(__FILE__) + '/../lib/haml'
require 'haml/template'
require 'sass/engine'
require 'rubygems'
require 'active_support'
require 'action_view'
require 'benchmark'
require 'markaby'
require 'stringio'
require 'open-uri'

module Haml
  class Benchmarker
  
    # Creates a new benchmarker that looks for templates in the base
    # directory.
    def initialize(base = File.dirname(__FILE__))
      ActionView::Base.register_template_handler("haml", Haml::Template)
      ActionView::Base.register_template_handler("mab", Markaby::Template)
      unless base.class == ActionView::Base
        @base = ActionView::Base.new(base)
      else
        @base = base
      end
    end
    
    # Benchmarks haml against ERb, and Sass on its own.
    # 
    # Returns the results of the benchmarking as a string.
    # 
    def benchmark(runs = 100)
      template_name = 'standard'
      haml_template = "haml/templates/#{template_name}"
      rhtml_template = "haml/rhtml/#{template_name}"
      markaby_template = File.dirname(__FILE__) + "/haml/markaby/#{template_name}.mab"
      markaby_template_data = open(markaby_template).read
      sass_template = File.dirname(__FILE__) + "/sass/templates/complex.sass"
      
      old_stdout = $stdout
      $stdout = StringIO.new

      times = Benchmark.bmbm do |b|
        b.report("haml:") { runs.times { @base.render haml_template } }
        b.report("erb:") { runs.times { @base.render rhtml_template } }
        b.report("mab:") { runs.times { 
          Markaby::Template.new(markaby_template_data).render
          #@base.render markaby_template 
        } }
      end
      
      #puts times[0].inspect, times[1].inspect
      ratio = sprintf("%g", times[0].to_a[5] / times[1].to_a[5])
      mab_ratio = sprintf("%g", times[2].to_a[5] / times[1].to_a[5])
      puts "Haml/ERB: " + ratio
      puts "Mab/Haml: " + mab_ratio
      
      puts '', '-' * 50, 'Sass on its own', '-' * 50
      
      Benchmark.bmbm do |b|
        b.report("sass:") { runs.times { Sass::Engine.new(File.read(sass_template)).render } }
      end
      
      $stdout.pos = 0
      to_return = $stdout.read
      $stdout = old_stdout
      
      to_return
    end
  end
end
