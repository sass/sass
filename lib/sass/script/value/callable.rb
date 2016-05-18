module Sass::Script::Value
  # A SassScript object representing a null value.
  class Callable < Base
    def initialize(callable)
      super(callable.dup)
    end

    def to_s(opts = {})
      raise Sass::SyntaxError.new("#{to_sass} isn't a valid CSS value.")
    end

    def inspect
      to_sass
    end

    def to_sass
      "#{value.type}-reference(#{value.name})"
    end

    def plus(other)
      raise Sass::SyntaxError.new("Addition is not defined for values of type: Callable")
    end

    def minus(other)
      raise Sass::SyntaxError.new("Subtraction is not defined for values of type: Callable")
    end

    def div(other)
      raise Sass::SyntaxError.new("Division is not defined for values of type: Callable")
    end

    def unary_plus
      raise Sass::SyntaxError.new("Positive is not defined for values of type: Callable")
    end

    def unary_minus
      raise Sass::SyntaxError.new("Negation is not defined for values of type: Callable")
    end

    def unary_div
      raise Sass::SyntaxError.new("Division is not defined for values of type: Callable")
    end
  end
end
