require 'sass/tree/node'

module Sass::Tree
  class MixinNode < Node
    def initialize(name, args)
      @name = name
      @args = args
      super()
    end

    protected

    def _perform(environment)
      raise Sass::SyntaxError.new("Undefined mixin '#{@name}'.", @line) unless mixin = environment.mixin(@name)

      raise Sass::SyntaxError.new(<<END.gsub("\n", "")) if mixin.args.size < @args.size
Mixin #{@name} takes #{mixin.args.size} argument#{'s' if mixin.args.size != 1}
 but #{@args.size} #{@args.size == 1 ? 'was' : 'were'} passed.
END

      environment = mixin.args.zip(@args).
        inject(Sass::Environment.new(mixin.environment)) do |env, ((name, default), value)|
        env.set_local_var(name,
          if value
            value.perform(environment)
          elsif default
            default.perform(env)
          end)
        raise Sass::SyntaxError.new("Mixin #{@name} is missing parameter !#{name}.") unless env.var(name)
        env
      end
      mixin.tree.map {|c| c.perform(environment)}.flatten
    end
  end
end
