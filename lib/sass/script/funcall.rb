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
        keywords = Sass::Util.hash_to_a(@keywords).
            map {|k, v| "$#{k}: #{v.inspect}"}.join(', ')
        "#{name}(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords})"
      end

      # @see Node#to_sass
      def to_sass(opts = {})
        args = @args.map {|a| a.to_sass(opts)}.join(', ')
        keywords = Sass::Util.hash_to_a(@keywords).
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

      # @see Node#deep_copy
      def deep_copy
        node = dup
        node.instance_variable_set('@args', args.map {|a| a.deep_copy})
        node.instance_variable_set('@keywords', Hash[keywords.map {|k, v| [k, v.deep_copy]}])
        node
      end

      protected

      # Evaluates the function call.
      #
      # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
      # @return [Literal] The SassScript object that is the value of the function call
      # @raise [Sass::SyntaxError] if the function call raises an ArgumentError
      def _perform(environment)
        args = @args.map {|a| a.perform(environment)}
        if fn = environment.function(@name)
          keywords = Sass::Util.map_hash(@keywords) {|k, v| [k, v.perform(environment)]}
          return perform_sass_fn(fn, args, keywords)
        end

        ruby_name = @name.tr('-', '_')
        args = construct_ruby_args(ruby_name, args, environment)

        unless Functions.callable?(ruby_name)
          opts(to_literal(args))
        else
          opts(Functions::EvaluationContext.new(environment.options).send(ruby_name, *args))
        end
      rescue ArgumentError => e
        # If this is a legitimate Ruby-raised argument error, re-raise it.
        # Otherwise, it's an error in the user's stylesheet, so wrap it.
        if e.message =~ /^wrong number of arguments \(\d+ for \d+\)/ &&
            e.backtrace[0] !~ /:in `(block in )?#{ruby_name}'$/
          raise e
        end
        raise Sass::SyntaxError.new("#{e.message} for `#{name}'")
      end

      # This method is factored out from `_perform` so that compass can override
      # it with a cross-browser implementation for functions that require vendor prefixes
      # in the generated css.
      def to_literal(args)
        Script::String.new("#{name}(#{args.join(', ')})")
      end

      private

      def construct_ruby_args(name, args, environment)
        unless signature = Functions.signature(name.to_sym, args.size, @keywords.size)
          return args if keywords.empty?
          raise Sass::SyntaxError.new("Function #{name} doesn't support keyword arguments")
        end
        keywords = Sass::Util.map_hash(@keywords) {|k, v| [k, v.perform(environment)]}

        # If the user passes more non-keyword args than the function expects,
        # but it does expect keyword args, Ruby's arg handling won't raise an error.
        # Since we don't want to make functions think about this,
        # we'll handle it for them here.
        if signature.var_kwargs && !signature.var_args && args.size > signature.args.size
          raise Sass::SyntaxError.new(
            "#{args[signature.args.size].inspect} is not a keyword argument for `#{name}'")
        elsif keywords.empty?
          return args 
        end

        args = args + signature.args[args.size..-1].map do |argname|
          if keywords.has_key?(argname)
            keywords.delete(argname)
          else
            raise Sass::SyntaxError.new("Function #{name} requires an argument named $#{argname}")
          end
        end

        if keywords.size > 0
          if signature.var_kwargs
            args << keywords
          else
            raise Sass::SyntaxError.new("Function #{name} doesn't take an argument named $#{keywords.keys.sort.first}")
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

        if args.size > function.args.size
          raise ArgumentError.new("Wrong number of arguments (#{args.size} for #{function.args.size})")
        end

        environment = function.args.zip(args).
          inject(Sass::Environment.new(function.environment)) do |env, ((var, default), value)|
          env.set_local_var(var.name,
            value || keywords[var.underscored_name] || (default && default.perform(env)))
          raise Sass::SyntaxError.new("Function #{@name} is missing parameter #{var.inspect}.") unless env.var(var.name)
          env
        end

        val = catch :_sass_return do
          function.tree.each {|c| Sass::Tree::Visitors::Perform.visit(c, environment)}
          raise Sass::SyntaxError.new("Function #{@name} finished without @return")
        end
        val
      end
    end
  end
end
