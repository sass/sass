require 'sass/script/functions'
require 'sass/util/normalized_map'

module Sass::Script::Tree
  # A SassScript parse node representing a function call.
  #
  # A function call either calls one of the functions in
  # {Sass::Script::Functions}, or if no function with the given name exists it
  # returns a string representation of the function call.
  class Funcall < Node
    # The name of the function.
    #
    # @return [String]
    attr_reader :name

    # The arguments to the function.
    #
    # @return [Array<Node>]
    attr_reader :args

    # The keyword arguments to the function.
    #
    # @return [Sass::Util::NormalizedMap<Node>]
    attr_reader :keywords

    # The first splat argument for this function, if one exists.
    #
    # This could be a list of positional arguments, a map of keyword
    # arguments, or an arglist containing both.
    #
    # @return [Node?]
    attr_accessor :splat

    # The second splat argument for this function, if one exists.
    #
    # If this exists, it's always a map of keyword arguments, and
    # \{#splat} is always either a list or an arglist.
    #
    # @return [Node?]
    attr_accessor :kwarg_splat

    # @param name [String] See \{#name}
    # @param args [Array<Node>] See \{#args}
    # @param keywords [Sass::Util::NormalizedMap<Node>] See \{#keywords}
    # @param splat [Node] See \{#splat}
    # @param kwarg_splat [Node] See \{#kwarg_splat}
    def initialize(name, args, keywords, splat, kwarg_splat)
      @name = name
      @args = args
      @keywords = keywords
      @splat = splat
      @kwarg_splat = kwarg_splat
      super()
    end

    # @return [String] A string representation of the function call
    def inspect
      args = @args.map {|a| a.inspect}.join(', ')
      keywords = Sass::Util.hash_to_a(@keywords.as_stored).
          map {|k, v| "$#{k}: #{v.inspect}"}.join(', ')
      # rubocop:disable RedundantSelf
      if self.splat
        splat = args.empty? && keywords.empty? ? "" : ", "
        splat = "#{splat}#{self.splat.inspect}..."
        splat = "#{splat}, #{kwarg_splat.inspect}..." if kwarg_splat
      end
      # rubocop:enable RedundantSelf
      "#{name}(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords}#{splat})"
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      arg_to_sass = lambda do |arg|
        sass = arg.to_sass(opts)
        sass = "(#{sass})" if arg.is_a?(Sass::Script::Tree::ListLiteral) && arg.separator == :comma
        sass
      end

      args = @args.map(&arg_to_sass)
      keywords = Sass::Util.hash_to_a(@keywords.as_stored).
        map {|k, v| "$#{dasherize(k, opts)}: #{arg_to_sass[v]}"}

      # rubocop:disable RedundantSelf
      if self.splat
        splat = "#{arg_to_sass[self.splat]}..."
        kwarg_splat = "#{arg_to_sass[self.kwarg_splat]}..." if self.kwarg_splat
      end
      # rubocop:enable RedundantSelf

      arglist = [args, splat, keywords, kwarg_splat].flatten.compact.join(', ')
      "#{dasherize(name, opts)}(#{arglist})"
    end

    # Returns the arguments to the function.
    #
    # @return [Array<Node>]
    # @see Node#children
    def children
      res = @args + @keywords.values
      res << @splat if @splat
      res << @kwarg_splat if @kwarg_splat
      res
    end

    # @see Node#deep_copy
    def deep_copy
      node = dup
      node.instance_variable_set('@args', args.map {|a| a.deep_copy})
      copied_keywords = Sass::Util::NormalizedMap.new
      @keywords.as_stored.each {|k, v| copied_keywords[k] = v.deep_copy}
      node.instance_variable_set('@keywords', copied_keywords)
      node
    end

    protected

    def _to_sexp(visitor)
      if Sass::Script::Functions.callable?(ruby_name)
        if (signature = Sass::Script::Functions.signature(
              ruby_name.to_sym, args.size, keywords.size))
          # A splat may contain keyword arguments, so we fall back on
          # dynamic function resolution to ensure that they're matched
          # with the right Ruby arguments.
          return ruby_function_call(visitor, signature) unless splat
        elsif !keywords.empty? || kwarg_splat
          return sass_error(s(:str, "Function #{name} doesn't support keyword arguments"))
        else
          return simple_ruby_function_call(visitor)
        end
      end

      variable, function = visitor.environment.fn_variable(name)
      return visitor.run_callable(variable, function, self, "function #{name}")
    end

    def ruby_function_call(visitor, signature)
      # If the user passes more non-keyword args than the function expects,
      # but it does expect keyword args, Ruby's arg handling won't raise an error.
      # Since we don't want to make functions think about this,
      # we'll handle it for them here.
      if signature.var_kwargs && !signature.var_args && args.size > signature.args.size
        # TODO: evaluate other args for side effects
        return sass_error(s(:dstr, '',
          s(:evstr, to_string(args[signature.args.size].to_sexp(visitor))),
          s(:str, " is not a keyword argument for `#{name}'")))
      elsif keywords.empty? && !kwarg_splat
        if signature.macro
          return Sass::Script::Functions.send(ruby_name, visitor, *args.map {|a| a.to_sexp(visitor)})
        else
          return simple_ruby_function_call(visitor)
        end
      end

      argnames = signature.args[args.size..-1] || []
      deprecated_argnames = (signature.deprecated && signature.deprecated[args.size..-1]) || []
      arg_sexps = args.map do |arg|
        arg.to_sexp(visitor)
      end + argnames.zip(deprecated_argnames).map do |(argname, deprecated_argname)|
        if keywords.has_key?(argname)
          keywords.delete(argname).to_sexp(visitor)
        elsif deprecated_argname && keywords.has_key?(deprecated_argname)
          deprecated_argname = keywords.denormalize(deprecated_argname)
          s(:block,
            s(:call, sass(:Util), :sass_warn,
              s(:str, "DEPRECATION WARNING: The `$#{deprecated_argname}' argument for " +
                      "`#{@name}()' has been renamed to `$#{argname}'.")),
            keywords.delete(deprecated_argname).to_sexp(visitor))
        else
          sass_error(s(:str, "Function #{name} requires an argument named $#{argname}"))
        end
      end

      if keywords.size > 0
        if signature.var_kwargs
          # Don't pass a NormalizedMap to a Ruby function.
          arg_sexps << s(:hash,
            *Sass::Util.flatten(keywords.map {|k, v| [s(:lit, k), v.to_sexp(visitor)]}, 1))
        else
          argname = keywords.keys.sort.first
          if signature.args.include?(argname)
            return sass_error(s(:str,
              "Function #{name} was passed argument $#{argname} both by position and by name"))
          else
            return sass_error(s(:str,
              "Function #{name} doesn't have an argument named $#{argname}"))
          end
        end
      end

      
      return Sass::Script::Functions.send(ruby_name, visitor, *arg_sexps) if signature.macro

      s(:rescue, s(:call, s(:self), ruby_name, *arg_sexps),
        resbody(s(:const, :ArgumentError), :_s_error,
          s(:call, sass(:Script, :Helpers), :reformat_argument_error,
            s(:str, ruby_name), s(:str, name), s(:lvar, :_s_error))))
    end

    def simple_ruby_function_call(visitor)
      call = s(:call, s(:self), ruby_name, *args.map {|a| a.to_sexp(visitor)})
      # TODO: throw an error if the splat contains kwargs
      call << s(:splat, s(:call, splat.to_sexp(visitor), :to_a)) if splat
      s(:rescue, call,
        resbody(s(:const, :ArgumentError), :_s_error,
          s(:call, sass(:Script, :Helpers), :reformat_argument_error,
            s(:str, ruby_name), s(:str, name), s(:lvar, :_s_error))))
    end

    # Evaluates the function call.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Sass::Script::Value] The SassScript object that is the value of the function call
    # @raise [Sass::SyntaxError] if the function call raises an ArgumentError
    def _perform(environment)
      args = Sass::Util.enum_with_index(@args).
        map {|a, i| perform_arg(a, environment, signature && signature.args[i])}
      keywords = Sass::Util.map_hash(@keywords) do |k, v|
        [k, perform_arg(v, environment, k.tr('-', '_'))]
      end
      splat = Sass::Tree::Visitors::Perform.perform_splat(
        @splat, keywords, @kwarg_splat, environment)
      if (fn = environment.function(@name))
        return without_original(perform_sass_fn(fn, args, splat, environment))
      end

      args = construct_ruby_args(ruby_name, args, splat, environment)

      if Sass::Script::Functions.callable?(ruby_name)
        local_environment = Sass::Environment.new(environment.global_env, environment.options)
        local_environment.caller = Sass::ReadOnlyEnvironment.new(environment, environment.options)
        result = opts(Sass::Script::Functions::EvaluationContext.new(
          local_environment).send(ruby_name, *args))
        without_original(result)
      else
        opts(to_literal(args))
      end
    rescue ArgumentError => e
      reformat_argument_error(e)
    end

    # Compass historically overrode this before it changed name to {Funcall#to_value}.
    # We should get rid of it in the future.
    def to_literal(args)
      to_value(args)
    end

    # This method is factored out from `_perform` so that compass can override
    # it with a cross-browser implementation for functions that require vendor prefixes
    # in the generated css.
    def to_value(args)
      Sass::Script::Value::String.new("#{name}(#{args.join(', ')})")
    end

    private

    def ruby_name
      @ruby_name ||= @name.tr('-', '_')
    end

    def perform_arg(argument, environment, name)
      return argument if signature && signature.delayed_args.include?(name)
      argument.perform(environment)
    end

    def signature
      @signature ||= Sass::Script::Functions.signature(name.to_sym, @args.size, @keywords.size)
    end

    def without_original(value)
      return value unless value.is_a?(Sass::Script::Value::Number)
      value = value.dup
      value.original = nil
      value
    end

    def construct_ruby_args(name, args, splat, environment)
      args += splat.to_a if splat

      # All keywords are contained in splat.keywords for consistency,
      # even if there were no splats passed in.
      old_keywords_accessed = splat.keywords_accessed
      keywords = splat.keywords
      splat.keywords_accessed = old_keywords_accessed

      unless (signature = Sass::Script::Functions.signature(name.to_sym, args.size, keywords.size))
        return args if keywords.empty?
        raise Sass::SyntaxError.new("Function #{name} doesn't support keyword arguments")
      end

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

      argnames = signature.args[args.size..-1] || []
      deprecated_argnames = (signature.deprecated && signature.deprecated[args.size..-1]) || []
      args = args + argnames.zip(deprecated_argnames).map do |(argname, deprecated_argname)|
        if keywords.has_key?(argname)
          keywords.delete(argname)
        elsif deprecated_argname && keywords.has_key?(deprecated_argname)
          deprecated_argname = keywords.denormalize(deprecated_argname)
          Sass::Util.sass_warn("DEPRECATION WARNING: The `$#{deprecated_argname}' argument for " +
            "`#{@name}()' has been renamed to `$#{argname}'.")
          keywords.delete(deprecated_argname)
        else
          raise Sass::SyntaxError.new("Function #{name} requires an argument named $#{argname}")
        end
      end

      if keywords.size > 0
        if signature.var_kwargs
          # Don't pass a NormalizedMap to a Ruby function.
          args << keywords.to_hash
        else
          argname = keywords.keys.sort.first
          if signature.args.include?(argname)
            raise Sass::SyntaxError.new(
              "Function #{name} was passed argument $#{argname} both by position and by name")
          else
            raise Sass::SyntaxError.new(
              "Function #{name} doesn't have an argument named $#{argname}")
          end
        end
      end

      args
    end

    def perform_sass_fn(function, args, splat, environment)
      Sass::Tree::Visitors::Perform.perform_arguments(function, args, splat, environment) do |env|
        env.caller = Sass::Environment.new(environment)

        val = catch :_sass_return do
          function.tree.each {|c| Sass::Tree::Visitors::Perform.visit(c, env)}
          raise Sass::SyntaxError.new("Function #{@name} finished without @return")
        end
        val
      end
    end
  end
end
