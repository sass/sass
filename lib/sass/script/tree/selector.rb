module Sass::Script::Tree
  # A SassScript node that will resolve to the current selector.
  class Selector < Node
    def initialize; end

    def children
      []
    end

    def to_sass(opts = {})
      '&'
    end

    def deep_copy
      dup
    end

    protected

    def _to_sexp(visitor)
      selector_var = visitor.environment.unique_ident(:selector)
      s(:block,
        s(:lasgn, selector_var, s(:call, s(:lvar, :_s_env), :selector)),
        s(:if, s(:lvar, selector_var), s(:call, s(:lvar, selector_var), :to_sass_script),
          Sass::Script::Value::Null.new.to_sexp))
    end

    def _perform(environment)
      selector = environment.selector
      return opts(Sass::Script::Value::Null.new) unless selector
      opts(selector.to_sass_script)
    end
  end
end
