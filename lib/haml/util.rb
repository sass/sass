require 'erb'
require 'set'

module Haml
  module Util
    class << self; include Haml::Util; end

    def to_hash(arr)
      arr.compact.inject({}) {|h, (k, v)| h[k] = v; h}
    end

    def map_keys(hash)
      to_hash(hash.map {|k, v| [yield(k), v]})
    end

    def map_vals(hash)
      to_hash(hash.map {|k, v| [k, yield(v)]})
    end

    def map_hash(hash, &block)
      to_hash(hash.map(&block))
    end

    def powerset(arr)
      arr.inject([Set.new].to_set) do |powerset, el|
        new_powerset = Set.new
        powerset.each do |subset|
          new_powerset << subset
          new_powerset << subset + [el]
        end
        new_powerset
      end
    end

    class StaticConditionalContext
      def initialize(set)
        @set = set
      end

      def method_missing(name, *args, &block)
        super unless args.empty? && block.nil?
        @set.include?(name)
      end
    end

    def def_static_method(klass, name, args, *vars)
      erb = vars.pop
      powerset(vars).each do |set|
        context = StaticConditionalContext.new(set).instance_eval {binding}
        klass.class_eval(<<METHOD)
def #{static_method_name(name, *vars.map {|v| set.include?(v)})}(#{args.join(', ')})
  #{ERB.new(erb).result(context)}
end
METHOD
      end
    end

    def static_method_name(name, *vars)
      "#{name}_#{vars.map {|v| !!v}.join('_')}"
    end
  end
end
