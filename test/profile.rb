require 'rubygems'
require 'active_support'
require 'action_controller'
require 'action_view'

require File.dirname(__FILE__) + '/../lib/haml'
require 'haml/template'

require 'profiler'
require 'stringio'

module Haml
  # Used by both Haml::Profiler and Sass::Profiler.
  # Encapsulates profiling behavior.
  module AbstractProfiler
    def self.profile(times, &block)
      # Runs the profiler, collects information
      Profiler__::start_profile
      times.times &block
      Profiler__::stop_profile
      
      # Outputs information to a StringIO, returns result
      io = StringIO.new
      Profiler__::print_profile(io)
      io.pos = 0
      result = io.read
      io.close
      result
    end
  end

  # A profiler for Haml, mostly for development use. This simply implements
  # the Ruby profiler for profiling haml code.
  class Profiler
  
    # Creates a new profiler that looks for templates in the base
    # directory.
    def initialize(base = File.join(File.dirname(__FILE__), 'haml', 'templates'))
      unless base.class == ActionView::Base
        @base = ActionView::Base.new(base)
      else
        @base = base
      end
    end

    # Profiles haml on the given template with the given number of runs.
    # The template name shouldn't have a file extension; this will
    # automatically look for a haml template.
    # 
    # Returns the results of the profiling as a string.
    def profile(runs = 100, template_name = 'standard')      
      AbstractProfiler.profile(runs) { @base.render template_name }
    end
  end
end

module Sass
  class Profiler
    def profile(runs = 100, template_name = 'complex')
      Haml::AbstractProfiler.profile(runs) do
        Sass::Engine.new("#{File.dirname(__FILE__)}/sass/templates/#{template_name}.sass").render
      end
    end
  end
end
