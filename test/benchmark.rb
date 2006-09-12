require File.dirname(__FILE__) + '/../lib/haml/engine'

$:.unshift File.join(File.dirname(__FILE__), "..", "lib")

require 'rubygems'
require 'action_view'

include Haml::Helpers

ActionView::Base.register_template_handler("haml", Haml::Engine)
@base = ActionView::Base.new(File.dirname(__FILE__))

RUNS = (ARGV[0] || 100).to_i

Benchmark.bm do |b|
  b.report("haml: ") { RUNS.times { @base.render "templates/standard" } }
  b.report("rhtml:") { RUNS.times { @base.render "rhtml/standard" } }
end
