require 'sass/constant/string'
require 'sass/constant/number'
require 'sass/constant/color'
require 'sass/constant/functions'
require 'sass/constant/unary_operation'

module Sass::Constant
  class Operation # :nodoc:
    def initialize(operand1, operand2, operator)
      raise Sass::SyntaxError.new("SassScript doesn't support a single-& operator.") if operator == :single_and
      raise Sass::SyntaxError.new("SassScript doesn't support a single-| operator.") if operator == :single_or
      raise Sass::SyntaxError.new("SassScript doesn't support a single-= operator.") if operator == :single_equals

      @operand1 = operand1
      @operand2 = operand2
      @operator = operator
    end

    def to_s
      self.perform.to_s
    end

    def inspect
      "(#{@operator.inspect} #{@operand1.inspect} #{@operand2.inspect})"
    end

    def to_arglist
      return [self] unless @operator == :comma
      @operand1.to_arglist + @operand2.to_arglist
    end

    def perform
      literal1 = @operand1.perform
      if @operator == :funcall && Functions.public_instance_methods.include?(literal1.to_s) && literal1.to_s !~ /^__/
        begin
          return Functions.send(literal1.to_s, *@operand2.to_arglist)
        rescue ArgumentError => e
          raise e unless e.backtrace.first =~ /:in `#{literal1.to_s}'$/
          raise Sass::SyntaxError.new("#{e.message} for `#{literal1.to_s}'")
        end
      end

      literal2 = @operand2.perform
      begin
        literal1.send(@operator, literal2)
      rescue NoMethodError => e
        raise e unless e.name.to_s == @operator.to_s
        raise Sass::SyntaxError.new("Undefined operation: \"#{literal1} #{@operator} #{literal2}\".")
      end
    end
  end
end
