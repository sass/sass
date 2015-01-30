module Sass
  class RuntimeEnvironment
    # TODO: to simulate the old Environment, this should somehow
    # contain the current filename.
    attr_reader :options

    attr_accessor :selector

    attr_writer :context

    def initialize(options)
      @options = options
    end

    # The import/mixin stack.
    #
    # @return [Sass::Stack]
    def stack
      @context.stack
    end

    def global?
      raise NotImplementedError.new("RuntimeEnvironment#global? is not yet implemented.")
    end

    def caller
      raise NotImplementedError.new("RuntimeEnvironment#caller is not yet implemented.")
    end

    def content
      raise NotImplementedError.new("RuntimeEnvironment#content is not yet implemented.")
    end

    def var(name)
      raise NotImplementedError.new("RuntimeEnvironment#var is not yet implemented.")
    end

    def is_var_global?(name)
      raise NotImplementedError.new("RuntimeEnvironment#is_var_global? is not yet implemented.")
    end

    def set_global_var(name, value)
      @context.instance_variable_set("@" + Sass::Util.consistent_ident("var_#{name}"), value)
    end

    def mixin(name)
      raise NotImplementedError.new("RuntimeEnvironment#mixin is not yet implemented.")
    end

    def is_mixin_global?(name)
      raise NotImplementedError.new("RuntimeEnvironment#is_mixin_global? is not yet implemented.")
    end

    def set_global_mixin(name, value)
      raise NotImplementedError.new("RuntimeEnvironment#set_global_mixin is not yet implemented.")
    end

    def function(name)
      raise NotImplementedError.new("RuntimeEnvironment#function is not yet implemented.")
    end

    def is_function_global?(name)
      raise NotImplementedError.new("RuntimeEnvironment#is_function_global? is not yet implemented.")
    end

    def set_global_function(name, value)
      raise NotImplementedError.new("RuntimeEnvironment#set_global_function is not yet implemented.")
    end

    private

    def run_function(name, splat)
      callable = fn_signatures[name]

      if callable.nil?
        ruby_name = name.tr('-', '_')
        if Sass::Script::Functions.callable?(ruby_name)
          return Sass::Script::Helpers.without_original(
            run_ruby_function(name, ruby_name, splat))
        end
      end

      if callable
        return Sass::Script::Helpers.without_original(run_callable(callable, splat))
      end

      # TODO: throw an error if splat has keywords.
      Sass::Script::Value::String.new("#{name}(#{splat.to_a.join(', ')})")
    end

    def run_mixin(name, splat)
      callable = mx_signatures[name]
      return run_callable(callable, splat) if callable
      raise Sass::SyntaxError.new("Undefined mixin '#{name}'.")
    end

    def run_callable(callable, splat)
      # TODO: test all calling convention stuff twice, once for static
      # and once for dynamic calls.
      desc = "#{callable.type.capitalize} #{callable.name}"
      downcase_desc = "#{callable.type} #{callable.name}"

      # All keywords are contained in splat.keywords for consistency,
      # even if there were no splats passed in.
      keywords = splat.keywords_safe

      begin
        unless keywords.empty?
          unknown_args = Sass::Util.array_minus(keywords.keys,
            callable.args.map {|var| var && var.first.underscored_name}.compact)
          if callable.splat && unknown_args.include?(callable.splat.underscored_name)
            raise Sass::SyntaxError.new("Argument $#{callable.splat.name} of #{downcase_desc} " +
                                        "cannot be used as a named argument.")
          elsif unknown_args.any?
            description = unknown_args.length > 1 ? 'the following arguments:' : 'an argument named'
            raise Sass::SyntaxError.new("#{desc} doesn't have #{description} " +
                                        "#{unknown_args.map {|name| "$#{name}"}.join ', '}.")
          end
        end
      rescue Sass::SyntaxError => keyword_exception
      end

      # If there's no splat, raise the keyword exception immediately. The actual
      # raising happens in the ensure clause at the end of this function.
      return if keyword_exception && !callable.splat

      args = splat.to_a
      splat_sep = splat.separator

      if args.size > callable.args.size && !callable.splat
        extra_args_because_of_splat = splat && args.size - splat.to_a.size <= callable.args.size

        takes = callable.args.size
        passed = args.size
        message = "#{desc} takes #{takes} argument#{'s' unless takes == 1} " +
          "but #{passed} #{passed == 1 ? 'was' : 'were'} passed."
        raise Sass::SyntaxError.new(message) unless extra_args_because_of_splat
        # TODO: when the deprecation period is over, make this an error.
        Sass::Util.sass_warn("WARNING: #{message}\n" +
          stack.to_s.gsub(/^/m, " " * 8) + "\n" +
          "This will be an error in future versions of Sass.")
      end

      ruby_args = callable.args.zip(args[0...callable.args.length]).map do |(var, default), value|
        # var will be nil for Ruby functions without declared signatures.
        next value if var.nil?

        if value && keywords.has_key?(var.name)
          raise Sass::SyntaxError.new("#{desc} was passed argument $#{var.name} " +
                                      "both by position and by name.")
        end

        value ||= keywords.delete(var.name)
        raise Sass::SyntaxError.new("#{desc} is missing argument #{var.inspect}.") unless value
        value
      end

      if callable.splat
        rest = args[callable.args.length..-1] || []
        arg_list = Sass::Script::Value::ArgList.new(rest, keywords, splat_sep)
        ruby_args << arg_list
      end

      callable.run(@context, ruby_args)
    rescue StandardError => e
    ensure
      # If there's a keyword exception, we don't want to throw it immediately,
      # because the invalid keywords may be part of a glob argument that should be
      # passed on to another function. So we only raise it if we reach the end of
      # this function *and* the keywords attached to the argument list glob object
      # haven't been accessed.
      #
      # The keyword exception takes precedence over any Sass errors, but not over
      # non-Sass exceptions.
      if keyword_exception &&
          !(arg_list && arg_list.keywords_accessed) &&
          (e.nil? || e.is_a?(Sass::SyntaxError))
        raise keyword_exception
      elsif e
        raise e
      end
    end

    def run_ruby_function(name, ruby_name, splat)
      @context.send(ruby_name, *construct_ruby_args(name, ruby_name, splat))
    rescue ArgumentError => e
      Sass::Script::Helpers.reformat_argument_error(ruby_name, name, e)
    end

    def construct_ruby_args(name, ruby_name, splat)
      args = splat.value
      keywords = splat.keywords_safe

      unless (signature = Sass::Script::Functions.signature(
               ruby_name.to_sym, args.size, keywords.size))
        return splat.to_a if keywords.empty?
        raise Sass::SyntaxError.new("Function #{name} doesn't support keyword arguments.")
      end

      # If the user passes more non-keyword args than the function expects,
      # but it does expect keyword args, Ruby's arg handling won't raise an error.
      # Since we don't want to make functions think about this,
      # we'll handle it for them here.
      if signature.var_kwargs && !signature.var_args && args.size > signature.args.size
        raise Sass::SyntaxError.new(
          "#{args[signature.args.size].inspect} is not a keyword argument for `#{name}'.")
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
          raise Sass::SyntaxError.new("Function #{name} requires an argument named $#{argname}.")
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
  end
end
