require File.join(File.dirname(__FILE__), 'functions')
module Sass
  module Script
    # A SassScript parse node representing a function call.
    #
    # A function call either calls one of the functions in {Script::Functions},
    # or if no function with the given name exists
    # it returns a string representation of the function call.
    class Funcall < Node
      # The name of the function.
      #
      # @return [String]
      attr_reader :name

      # The arguments to the function.
      #
      # @return [Array<Script::Node>]
      attr_reader :args

      # @param name [String] See \{#name}
      # @param name [Array<Script::Node>] See \{#args}
      def initialize(name, args)
        @name = name
        @args = args
      end

      # @return [String] A string representation of the function call
      def inspect
        "#{name}(#{args.map {|a| a.inspect}.join(', ')})"
      end

      # Evaluates the function call.
      #
      # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
      # @return [Literal] The SassScript object that is the value of the function call
      # @raise [Sass::SyntaxError] if the function call raises an ArgumentError
      def perform(environment)
        args = self.args.map {|a| a.perform(environment)}
        ruby_name = name.gsub('-', '_')
        unless Haml::Util.has?(:public_instance_method, Functions, ruby_name) && ruby_name !~ /^__/
          return Script::String.new("#{name}(#{args.map {|a| a.perform(environment)}.join(', ')})")
        end

        result = Functions::EvaluationContext.new(environment.options).send(ruby_name, *args)
        result.options = environment.options
        return result
      rescue ArgumentError => e
        raise e unless e.backtrace.any? {|t| t =~ /:in `(block in )?(#{name}|perform)'$/}
        raise Sass::SyntaxError.new("#{e.message} for `#{name}'")
      end

      # Returns the arguments to the function.
      #
      # @return [Array<Node>]
      # @see Node#children
      def children
        @args
      end
    end
  end
end
