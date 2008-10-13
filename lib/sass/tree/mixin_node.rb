require 'sass/tree/node'

module Sass::Tree
  class MixinNode < Node
    def initialize(mixin, args, options)
      @mixin = mixin
      @args = args
      super(options)
      self.children = @mixin.tree
    end

    protected

    def _perform(environment)
      perform_children(@mixin.args.zip(@args).inject(environment.dup) do |env, (arg, value)|
          env[arg[:name]] = if value
                              value.perform(environment)
                            else
                              arg[:default_value]
                            end
          raise Sass::SyntaxError.new("Mixin #{@mixin.name} is missing parameter !#{arg[:name]}.") unless env[arg[:name]]
          env
        end)
    end
  end
end
