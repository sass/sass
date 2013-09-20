module Sass::Script::Tree
  # A SassScript node that will resolve to the current selector.
  class Selector < Node
    def initialize; end

    def children
      []
    end

    def to_sass(opts = {})
      '&'
    end

    def deep_copy
      dup
    end

    protected

    def _perform(environment)
      selector = environment.selector
      return opts(Sass::Script::Value::Null.new) unless selector
      opts(Sass::Script::Value::List.new(selector.members.map do |seq|
        Sass::Script::Value::List.new(seq.members.map do |component|
          Sass::Script::Value::String.new(component.to_s)
        end, :space)
      end, :comma))
    end
  end
end
