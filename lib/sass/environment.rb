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
      @stack = []
      @ignore_parent_stack = false
      set_var("important", Script::String.new("!important")) unless @parent
    end

    # The options hash.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @return [{Symbol => Object}]
    def options
      @options || (parent && parent.options) || {}
    end

    # Push lexical frame information onto the runtime stack.
    # @param frame_info [{Symbol => Object}]
    # Frame information has the following keys:
    #
    # `:filename`
    # : The name of the file in which the lexical scope changed.
    #
    # `:mixin`
    # : The name of the mixin in which the lexical scope changed,
    #   or `nil` if it wasn't within in a mixin.
    #
    # `:line`
    # : The line of the file on which the lexical scope changed. Never nil.
    #
    # `:import`
    # : Set to `true` when the lexical scope is changing due to an import.
    def push(frame_info)
      @stack.push frame_info
    end

    # Pop runtime frame information from the stack.
    def pop
      @stack.pop
    end

    # A list of the runtime stack frame information
    # The last element in the list was pushed onto the stack most recently.
    def stack
      prev = (!@ignore_parent_stack && parent && parent.stack) || []
      prev + @stack
    end

    # Temporarily assume the runtime stack that is passed in.
    # @param stk A stack value from another environment.
    def with_stack(stk)
      @stack, old_stack = stk, @stack
      @ignore_parent_stack = true
      yield self
    ensure
      @ignore_parent_stack = false
      @stack = old_stack
    end

    class << self
      private

      # Note: when updating this,
      # update haml/yard/inherited_hash.rb as well.
      def inherited_hash(name)
        class_eval <<RUBY, __FILE__, __LINE__ + 1
          def #{name}(name)
            _#{name}(name.gsub('_', '-'))
          end

          def _#{name}(name)
            @#{name}s[name] || @parent && @parent._#{name}(name)
          end
          protected :_#{name}

          def set_#{name}(name, value)
            name = name.gsub('_', '-')
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
            @#{name}s[name.gsub('_', '-')] = value
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
