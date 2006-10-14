require File.dirname(__FILE__) + '/../lib/haml/template'
require 'rubygems'
require 'active_support'
require 'action_view'
require 'profiler'
require 'stringio'

module Haml
  # A profiler for HAML, mostly for development use. This simply implements
  # the Ruby profiler for profiling HAML code.
  class Profiler
  
    # Creates a new profiler that looks for templates in the base
    # directory.
    def initialize(base = File.join(File.dirname(__FILE__), 'templates'))
      ActionView::Base.register_template_handler("haml", Haml::Template)
      unless base.class == ActionView::Base
        @base = ActionView::Base.new(base)
      else
        @base = base
      end
    end

    # Profiles HAML on the given template with the given number of runs.
    # The template name shouldn't have a file extension; this will
    # automatically look for a HAML template.
    # 
    # Returns the results of the profiling as a string.
    def profile(runs = 100, template_name = 'standard')      
      # Runs the profiler, collects information
      Profiler__::start_profile
      runs.times { @base.render template_name }
      Profiler__::stop_profile
      
      # Outputs information to a StringIO, returns result
      io = StringIO.new
      Profiler__::print_profile(io)
      io.pos = 0
      result = io.read
      io.close
      return result
    end

  end
end
