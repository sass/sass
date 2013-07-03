require 'sass/script/functions'

module Sass::Script::Tree
  # A SassScript parse node representing an inline if function.
  class IfFunction < Funcall

    # Create the IfFunction node
    #
    # @param args [Array<Node>] The arguments passed to the function.
    # @param keywords [{String => Node}] The keyword arguments passed to the function.
    # @raise ArgumentError when the three required arguments (condition, if_true, if_false) are not present.
    def initialize(args, keywords)
      super("if", args, keywords.dup, nil)
      if args.size > 3
        raise Sass::SyntaxError.new("#{args.size} arguments provided to if() but only 3 are accepted.")
      end

      raise Sass::SyntaxError.new("A condition is required for if()") unless condition
      raise Sass::SyntaxError.new("A true value is required for if()") unless true_expression
      raise Sass::SyntaxError.new("A false value is required for if()") unless false_expression

      if (unknown_keys = keywords.keys - %w(condition if_true if_false)).any?
        raise Sass::SyntaxError.new("$#{unknown_keys.first} is not allowed as an argument to if()")
      end
    end

    # An expression that determines which branch to execute
    # @return [Node]
    def condition
      args[0] || keywords["condition"]
    end

    # The expression that is performed if the condition is true
    # @return [Node]
    def true_expression
      args[1] || keywords["if-true"]
    end


    # The expression that is performed if the condition is false
    # @return [Node]
    def false_expression
      args[2] || keywords["if-false"]
    end

    # @see Node#deep_copy
    def deep_copy
      IfFunction.new(@args.map{|a| a.deep_copy}, @keywords.deep_copy)
    end

    protected

    # Evaluates the if function.
    #
    # @see Node#_perform
    def _perform(environment)
      if condition.perform(environment).to_bool
        true_expression.perform(environment)
      else
        false_expression.perform(environment)
      end
    end
  end
end
