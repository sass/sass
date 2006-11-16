require File.dirname(__FILE__) + '/../lib/haml/template'
require 'rubygems'
require 'active_support'
require 'action_view'
require 'benchmark'
require 'stringio'

module Haml
  class Benchmarker
  
    # Creates a new benchmarker that looks for templates in the base
    # directory.
    def initialize(base = File.dirname(__FILE__))
      ActionView::Base.register_template_handler("haml", Haml::Template)
      unless base.class == ActionView::Base
        @base = ActionView::Base.new(base)
      else
        @base = base
      end
    end
    
    # Benchmarks HAML against ERb. If <tt>template_name</tt> is specified,
    # looks for a haml template in ./templates and an rhtml template in
    # ./rhtml with the name <tt>template_name</tt>. Otherwise, uses
    # <tt>haml_template</tt> and <tt>rhtml_template</tt> as the location of
    # the templates.
    # 
    # Returns the results of the benchmarking as a string.
    # 
    # :call-seq:
    # benchmark(runs = 100, template_name = 'standard')
    # benchmark(runs = 100, haml_template, rhtml_template)
    # 
    def benchmark(runs = 100, template_name = 'standard', other_template = nil)
      if other_template.nil?
        haml_template = "templates/#{template_name}"
        rhtml_template = "rhtml/#{template_name}"
      else
        haml_template = template_name
        rhtml_template = other_template
      end
      
      old_stdout = $stdout
      $stdout = StringIO.new
      
      times = Benchmark.bmbm do |b|
        b.report("haml:") { runs.times { @base.render haml_template } }
        b.report("erb:") { runs.times { @base.render rhtml_template } }
      end
      
      #puts times.inspect
      ratio = sprintf("%g", times[0].to_a[5] / times[1].to_a[5])
      puts "Haml/ERB: " + ratio
      
      $stdout.pos = 0
      to_return = $stdout.read
      $stdout = old_stdout
      
      to_return
    end
  end
end
