module Sass
  # The lexical environment for SassScript.
  # This keeps track of variable and mixin definitions.
  #
  # A new environment is created for each level of Sass nesting.
  # This allows variables to be lexically scoped.
  # The new environment refers to the environment in the upper scope,
  # so it has access to variables defined in enclosing scopes,
  # but new variables are defined locally.
  #
  # Environment also keeps track of the {Engine} options
  # so that they can be made available to {Sass::Functions}.
  class Environment
    # The enclosing environment,
    # or nil if this is the global environment.
    #
    # @return [Environment]
    attr_reader :parent

    # @param parent [Environment] See \{#parent}
    # @param options [Hash<Symbol, Object>] An options hash;
    #   see [the Sass options documentation](../Sass.html#sass_options)
    def initialize(parent = nil, options = nil)
      @vars = {}
      @mixins = {}
      @parent = parent
      @options = options
    end

    # The options hash.
    # See [the Sass options documentation](../Sass.html#sass_options).
    #
    # @return [Hash<Symbol, Object>]
    def options
      @options || (parent && parent.options) || {}
    end

    class << self
      private

      # Note: when updating this,
      # update haml/yard/inherited_hash.rb as well.
      def inherited_hash(name)
        class_eval <<RUBY, __FILE__, __LINE__ + 1
          def #{name}(name)
            @#{name}s[name] || @parent && @parent.#{name}(name)
          end

          def set_#{name}(name, value)
            if @parent.nil? || @#{name}s.include?(name)
              @#{name}s[name] = value
            else
              @parent.set_#{name}(name, value)
            end
          end

          def set_local_#{name}(name, value)
            @#{name}s[name] = value
          end
RUBY
      end
    end

    # variable
    # Script::Literal
    inherited_hash :var
    # mixin
    # Engine::Mixin
    inherited_hash :mixin
  end
end
