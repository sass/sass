require File.dirname(__FILE__) + '/../lib/haml/engine'

$:.unshift File.join(File.dirname(__FILE__), "..", "lib")

require 'rubygems'
require 'action_view'

include Haml::Helper

ActionView::Base.register_template_handler("haml", Haml::Engine)
@base = ActionView::Base.new(File.dirname(__FILE__))

RUNS = 2000
Benchmark.bm do |x|
  x.report("haml: ")  { RUNS.times { @base.render("templates/standard"); } }
  x.report("rhtml:")  { RUNS.times { @base.render(    "rhtml/standard"); } }
end

