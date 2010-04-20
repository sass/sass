require 'sass/tree/node'

module Sass::Tree
  # A static node representing a mixin include.
  # When in a static tree, the sole purpose is to wrap exceptions
  # to add the mixin to the backtrace.
  #
  # @see Sass::Tree
  class MixinNode < Node
    # @see Node#options=
    def options=(opts)
      super
      @args.each {|a| a.context = :equals} if opts[:sass2]
    end

    # @param name [String] The name of the mixin
    # @param args [Array<Script::Node>] The arguments to the mixin
    def initialize(name, args)
      @name = name
      @args = args
      super()
    end

    # @see Node#cssize
    def cssize(extends, parent = nil)
      _cssize(extends, parent) # Pass on the parent even if it's not a MixinNode
    end

    protected

    # Returns an error message if the given child node is invalid,
    # and false otherwise.
    #
    # {ExtendNode}s are valid within {MixinNode}s.
    #
    # @param child [Tree::Node] A potential child node
    # @return [Boolean, String] Whether or not the child node is valid,
    #   as well as the error message to display if it is invalid
    def invalid_child?(child)
      super unless child.is_a?(ExtendNode)
    end

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      args = '(' + @args.map {|a| a.to_sass(opts)}.join(", ") + ')' unless @args.empty?
      "#{'  ' * tabs}#{fmt == :sass ? '+' : '@include '}#{dasherize(@name, opts)}#{args}#{semi fmt}\n"
    end

    # @see Node#_cssize
    def _cssize(extends, parent)
      children.map do |c|
        parent.check_child! c
        c.cssize(extends, parent)
      end.flatten
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:mixin => @name, :filename => filename, :line => line)
      e.add_backtrace(:filename => filename, :line => line)
      raise e
    end

    # Runs the mixin.
    #
    # @param environment [Sass::Environment] The lexical environment containing
    #   variable and mixin values
    # @raise [Sass::SyntaxError] if there is no mixin with the given name
    # @raise [Sass::SyntaxError] if an incorrect number of arguments was passed
    # @see Sass::Tree
    def perform!(environment)
      original_env = environment
      original_env.push_frame(:filename => filename, :line => line)
      original_env.prepare_frame(:mixin => @name)
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
            val = default.perform(env)
            if default.context == :equals && val.is_a?(Sass::Script::String)
              val = Sass::Script::String.new(val.value)
            end
            val
          end)
        raise Sass::SyntaxError.new("Mixin #{@name} is missing parameter #{var.inspect}.") unless env.var(var.name)
        env
      end

      self.children = mixin.tree.map {|c| c.perform(environment)}.flatten
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:mixin => @name, :line => @line)
      e.add_backtrace(:line => @line)
      raise e
    ensure
      original_env.pop_frame
    end
  end
end
