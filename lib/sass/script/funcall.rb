require 'sass/script/functions'

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

      # The keyword arguments to the function.
      #
      # @return [{String => Script::Node}]
      attr_reader :keywords

      # Don't set the context for child nodes if this is `url()`,
      # since `url()` allows quoted strings.
      #
      # @param context [Symbol]
      # @see Node#context=
      def context=(context)
        super unless @name == "url"
      end

      # @param name [String] See \{#name}
      # @param args [Array<Script::Node>] See \{#args}
      # @param keywords [{String => Script::Node}] See \{#keywords}
      def initialize(name, args, keywords)
        @name = name
        @args = args
        @keywords = keywords
        super()
      end

      # @return [String] A string representation of the function call
      def inspect
        args = @args.map {|a| a.inspect}.join(', ')
        keywords = @keywords.sort_by {|k, v| k}.
            map {|k, v| "$#{k}: #{v.inspect}"}.join(', ')
        "#{name}(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords})"
      end

      # @see Node#to_sass
      def to_sass(opts = {})
        args = @args.map {|a| a.to_sass(opts)}.join(', ')
        keywords = @keywords.sort_by {|k, v| k}.
          map {|k, v| "$#{dasherize(k, opts)}: #{v.to_sass(opts)}"}.join(', ')
        "#{dasherize(name, opts)}(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords})"
      end

      # Returns the arguments to the function.
      #
      # @return [Array<Node>]
      # @see Node#children
      def children
        @args + @keywords.values
      end

      protected

      # Evaluates the function call.
      #
      # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
      # @return [Literal] The SassScript object that is the value of the function call
      # @raise [Sass::SyntaxError] if the function call raises an ArgumentError
      def _perform(environment)
        args = @args.map {|a| a.perform(environment)}
        keywords = Sass::Util.map_hash(@keywords) {|k, v| [k, v.perform(environment)]}
        if fn = environment.function(@name)
          return perform_sass_fn(fn, args, keywords)
        end

        ruby_name = @name.tr('-', '_')
        args = construct_ruby_args(ruby_name, args, keywords)

        unless Sass::Util.has?(:public_instance_method, Functions, ruby_name) && ruby_name !~ /^__/
          opts(Script::String.new("#{name}(#{args.join(', ')})"))
        else
          opts(Functions::EvaluationContext.new(environment.options).send(ruby_name, *args))
        end
      rescue ArgumentError => e
        raise e unless e.backtrace.any? {|t| t =~ /:in `(block in )?(#{name}|perform)'$/}
        raise Sass::SyntaxError.new("#{e.message} for `#{name}'")
      end

      private

      def construct_ruby_args(name, args, keywords)
        return args if keywords.empty?
        unless signature = Functions.signature(name.to_sym, args.size, keywords.size)
          raise Sass::SyntaxError.new("Function #{name} doesn't support keyword arguments")
        end

        args = args + signature.args[args.size..-1].map do |argname|
          if keywords.has_key?(argname)
            keywords.delete(argname)
          else
            raise Sass::SyntaxError, "Function #{name} requires an argument named $#{argname}"
          end
        end

        if keywords.size > 0
          if signature.var_kwargs
            args << Sass::Util.map_hash(keywords) {|k, v| [k.to_sym, v]}
          else
            raise Sass::SyntaxError, "Function #{name} doesn't take an argument named $#{keywords.keys.sort.first}"
          end
        end

        args
      end

      def perform_sass_fn(function, args, keywords)
        # TODO: merge with mixin arg evaluation?
        keywords.each do |name, value|
          # TODO: Make this fast
          unless function.args.find {|(var, default)| var.underscored_name == name}
            raise Sass::SyntaxError.new("Function #{@name} doesn't have an argument named $#{name}")
          end
        end

        environment = function.args.zip(args).
          inject(Sass::Environment.new(function.environment)) do |env, ((var, default), value)|
          env.set_local_var(var.name,
            value || keywords[var.underscored_name] || (default && default.perform(env)))
          raise Sass::SyntaxError.new("Function #{@name} is missing parameter #{var.inspect}.") unless env.var(var.name)
          env
        end

        val = catch :_sass_return do
          function.tree.each {|c| c.perform(environment)}
          raise Sass::SyntaxError.new("Function #{@name} finished without @return")
        end
        val
      end
    end
  end
end
