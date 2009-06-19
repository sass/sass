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
  # so that they can be made available to {Sass::Script::Functions}.
  class Environment
    # The enclosing environment,
    # or nil if this is the global environment.
    #
    # @return [Environment]
    attr_reader :parent
    attr_writer :options

    # @param parent [Environment] See \{#parent}
    def initialize(parent = nil)
      @vars = {}
      @mixins = {}
      @parent = parent

      set_var("important", Script::String.new("!important")) unless @parent
    end

    # The options hash.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
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
            @#{name}s[name] = value unless try_set_#{name}(name, value)
          end

          def try_set_#{name}(name, value)
            if @#{name}s.include?(name)
              @#{name}s[name] = value
              true
            elsif @parent
              @parent.try_set_#{name}(name, value)
            else
              false
            end
          end
          protected :try_set_#{name}

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
