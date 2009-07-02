require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a Sass `@for` loop.
  #
  # @see Sass::Tree
  class ForNode < Node
    # @param var [String] The name of the loop variable
    # @param from [Script::Node] The parse tree for the initial expression
    # @param to [Script::Node] The parse tree for the final expression
    # @param exclusive [Boolean] Whether to include `to` in the loop
    #   or stop just before
    def initialize(var, from, to, exclusive)
      @var = var
      @from = from
      @to = to
      @exclusive = exclusive
      super()
    end

    protected

    # Runs the child nodes once for each time through the loop,
    # varying the variable each time.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @return [Array<Tree::Node>] The resulting static nodes
    # @see Sass::Tree
    def _perform(environment)
      from = @from.perform(environment)
      to = @to.perform(environment)
      from.assert_int!
      to.assert_int!

      to = to.coerce(from.numerator_units, from.denominator_units)
      range = Range.new(from.to_i, to.to_i, @exclusive)

      children = []
      environment = Sass::Environment.new(environment)
      range.each do |i|
        environment.set_local_var(@var, Sass::Script::Number.new(i, from.numerator_units, from.denominator_units))
        children += perform_children(environment)
      end
      children
    end
  end
end
