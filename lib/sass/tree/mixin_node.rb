require 'sass/tree/node'

module Sass::Tree
  # A dynamic node representing a mixin include.
  #
  # @see Sass::Tree
  class MixinNode < Node
    # @param name [String] The name of the mixin
    # @param args [Array<Script::Node>] The arguments to the mixin
    def initialize(name, args)
      @name = name
      @args = args
      super()
    end

    protected

    # Runs the mixin.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @return [Array<Tree::Node>] The resulting static nodes
    # @raise [Sass::SyntaxError] if there is no mixin with the given name
    # @raise [Sass::SyntaxError] if an incorrect number of arguments was passed
    # @see Sass::Tree
    def _perform(environment)
      raise Sass::SyntaxError.new("Undefined mixin '#{@name}'.") unless mixin = environment.mixin(@name)

      raise Sass::SyntaxError.new(<<END.gsub("\n", "")) if mixin.args.size < @args.size
Mixin #{@name} takes #{mixin.args.size} argument#{'s' if mixin.args.size != 1}
 but #{@args.size} #{@args.size == 1 ? 'was' : 'were'} passed.
END

      environment = mixin.args.zip(@args).
        inject(Sass::Environment.new(mixin.environment)) do |env, ((var, default), value)|
        env.set_local_var(var.name,
          if value
            value.perform(environment)
          elsif default
            default.perform(env)
          end)
        raise Sass::SyntaxError.new("Mixin #{@name} is missing parameter #{var.inspect}.") unless env.var(var.name)
        env
      end
      mixin.tree.map {|c| c.perform(environment)}.flatten
    end
  end
end
