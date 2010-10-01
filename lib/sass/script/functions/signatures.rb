require "sass/script/functions"

module Sass::Script::Functions
  module Signatures

    # Declare a sass signature for a ruby-defined function.
    #
    # Sass function signatures can be overloaded as long as they have different arities
    # by calling define on the same method name repeatedly. The first matching signature
    # for the calling arguments is chosen in the order they are defined.
    #
    # @param options
    #   `:args` - Array of Symbols or Strings. Required arguments for this method signature.
    #   `:var_args` - Boolean. Indicates whether additional unnamed arguments can be passed.
    #   `:var_kwargs` - Boolean. Indicates whether additional named arguments can be passed.
    #                   These values are passed as the last argument and as a hash
    #                   of {String => Literal}
    # @example
    #   define :rgba, :args => [:hex, :alpha]
    #   define :rgba, :args => [:red, :green, :blue, :alpha]
    #   define :accepts_anything, :var_args => true, :var_kwargs => true
    #   define :some_func, :args => [:foo, :bar, :baz], :var_kwargs => true
    def define(method_name, options)
      options[:args] ||= []
      options[:args].map!{|a| a.to_s }
      options[:var_args] ||= false
      options[:var_kwargs] ||= false
      @signatures ||= {}
      @signatures[method_name.to_sym] ||= []
      @signatures[method_name.to_sym] << options
    end

    # Determine the correct signature for the arity of the arguments
    # if none match, the first signature is returned for error messaging
    #
    # @param method_name The name of the ruby function to be called
    # @param arg_arity The number of unnamed arguments the function was invoked with
    # @param kwarg_arity The number of named arguments the function was invoked with
    #
    # @return The signature options for the matching signature that were passed to {define}
    def signature(method_name, arg_arity, kwarg_arity)
      return unless @signatures[method_name.to_sym]
      @signatures[method_name.to_sym].each do |signature|
        if signature[:args].size == arg_arity + kwarg_arity
          return signature
        elsif signature[:args].size < arg_arity + kwarg_arity
          # we have enough args but we need to figure out what is variable
          # and if the signature allows it
          t_arg_arity, t_kwarg_arity = arg_arity, kwarg_arity
          if signature[:args].size > t_arg_arity
            # we transfer some kwargs arity to args arity
            # if it does not have enough args -- assuming the names will work out.
            t_kwarg_arity -= (signature[:args].size - t_arg_arity)
            t_arg_arity = signature[:args].size
          end
          if (  t_arg_arity == signature[:args].size ||   t_arg_arity > signature[:args].size && signature[:var_args]  ) &&
             (t_kwarg_arity == 0                     || t_kwarg_arity > 0                     && signature[:var_kwargs])
            return signature
          end
        end
      end
      @signatures[method_name.to_sym].first
    end
  end

  extend Signatures

end
