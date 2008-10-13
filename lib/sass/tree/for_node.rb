require 'sass/tree/node'

module Sass::Tree
  class ForNode < Node
    def initialize(var, from, to, exclusive, options)
      @var = var
      @from = from
      @to = to
      @exclusive = exclusive
      super(options)
    end

    protected

    def _perform(environment)
      from = @from.perform(environment).to_i
      to = @to.perform(environment).to_i
      range = Range.new(from, to, @exclusive)

      children = []
      sub_env = environment.dup
      range.each {|i| children += perform_children(sub_env.merge(@var => Sass::Script::Number.new(i)))}
      children
    end
  end
end
