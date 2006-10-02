require 'rubygems'
require 'active_support'
require 'action_view'

require File.dirname(__FILE__) + '/../lib/haml/template'

ActionView::Base.register_template_handler("haml", Haml::Template)
@base = ActionView::Base.new(File.dirname(__FILE__))

RUNS = (ARGV[0] || 100).to_i

Benchmark.bm do |b|
  b.report("haml: ") { RUNS.times { @base.render "templates/standard" } }
  b.report("erb:  ") { RUNS.times { @base.render "rhtml/standard" } }
end
