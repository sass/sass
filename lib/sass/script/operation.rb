require 'set'
require 'sass/script/string'
require 'sass/script/number'
require 'sass/script/color'
require 'sass/script/functions'
require 'sass/script/unary_operation'
require 'sass/script/interpolation'
require 'sass/script/string_interpolation'

module Sass::Script
  # A SassScript parse node representing a binary operation,
  # such as `$a + $b` or `"foo" + 1`.
  class Operation < Node
    attr_reader :operand1
    attr_reader :operand2
    attr_reader :operator

    # @param operand1 [Script::Node] The parse-tree node
    #   for the right-hand side of the operator
    # @param operand2 [Script::Node] The parse-tree node
    #   for the left-hand side of the operator
    # @param operator [Symbol] The operator to perform.
    #   This should be one of the binary operator names in {Lexer::OPERATORS}
    def initialize(operand1, operand2, operator)
      @operand1 = operand1
      @operand2 = operand2
      @operator = operator
      super()
    end

    # @return [String] A human-readable s-expression representation of the operation
    def inspect
      "(#{@operator.inspect} #{@operand1.inspect} #{@operand2.inspect})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      pred = Sass::Script::Parser.precedence_of(@operator)
      o1 = operand_to_sass pred, @operand1, opts
      o2 = operand_to_sass pred, @operand2, opts
      sep =
        case @operator
        when :comma; ", "
        when :concat; " "
        else; " #{Lexer::OPERATORS_REVERSE[@operator]} "
        end
      "#{o1}#{sep}#{o2}"
    end

    # Returns the operands for this operation.
    #
    # @return [Array<Node>]
    # @see Node#children
    def children
      [@operand1, @operand2]
    end

    protected

    # Evaluates the operation.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the operation
    # @raise [Sass::SyntaxError] if the operation is undefined for the operands
    def _perform(environment)
      literal1 = @operand1.perform(environment)
      literal2 = @operand2.perform(environment)

      if @operator == :concat && context == :equals
        literal1 = Sass::Script::String.new(literal1.value) if literal1.is_a?(Sass::Script::String)
        literal2 = Sass::Script::String.new(literal2.value) if literal2.is_a?(Sass::Script::String)
      end

      begin
        res = literal1.send(@operator, literal2)
        res.options = environment.options
        res
      rescue NoMethodError => e
        raise e unless e.name.to_s == @operator.to_s
        raise Sass::SyntaxError.new("Undefined operation: \"#{literal1} #{@operator} #{literal2}\".")
      end
    end

    private

    def operand_to_sass(pred, op, opts = {})
      return "(#{op.to_sass(opts)})" if op.is_a?(Operation) &&
        Sass::Script::Parser.precedence_of(op.operator) < pred
      op.to_sass(opts)
    end
  end
end
