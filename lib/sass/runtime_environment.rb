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
      @context.instance_variable_set("@" + consistent_ident("var_#{name}"), value)
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
  end
end
