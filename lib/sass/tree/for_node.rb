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
      environment = Sass::Environment.new(environment)
      range.each do |i|
        environment.set_local_var(@var, Sass::Script::Number.new(i))
        children += perform_children(environment)
      end
      children
    end
  end
end
