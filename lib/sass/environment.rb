require 'set'

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
      @stack = [] unless parent
      @mixins_in_use = Set.new unless parent
      set_var("important", Script::String.new("!important")) unless @parent
    end

    # The options hash.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @return [{Symbol => Object}]
    def options
      @options || (parent && parent.options) || {}
    end

    # Push a new stack frame onto the mixin/include stack.
    #
    # @param frame_info [{Symbol => Object}]
    #   Frame information has the following keys:
    #
    #   `:filename`
    #   : The name of the file in which the lexical scope changed.
    #
    #   `:mixin`
    #   : The name of the mixin in which the lexical scope changed,
    #     or `nil` if it wasn't within in a mixin.
    #
    #   `:line`
    #   : The line of the file on which the lexical scope changed. Never nil.
    def push_frame(frame_info)
      if stack.last && stack.last[:prepared]
        stack.last.delete(:prepared)
        stack.last.merge!(frame_info)
      else
        stack.push(frame_info)
      end
      mixins_in_use << stack.last[:mixin] if stack.last[:mixin] && !stack.last[:prepared]
    end

    # Like \{#push\_frame}, but next time a stack frame is pushed,
    # it will be merged with this frame.
    #
    # @param frame_info [{Symbol => Object}] Same as for \{#push\_frame}.
    def prepare_frame(frame_info)
      push_frame(frame_info.merge(:prepared => true))
    end

    # Pop a stack frame from the mixin/include stack.
    def pop_frame
      stack.pop if stack.last && stack.last[:prepared]
      popped = stack.pop
      mixins_in_use.delete(popped[:mixin]) if popped && popped[:mixin]
    end

    # A list of stack frames in the mixin/include stack.
    # The last element in the list is the most deeply-nested frame.
    #
    # @return [Array<{Symbol => Object}>] The stack frames,
    #   of the form passed to \{#push\_frame}.
    def stack
      @stack ||= @parent.stack
    end

    # A set of names of mixins currently present in the stack.
    #
    # @return [Set<String>] The mixin names.
    def mixins_in_use
      @mixins_in_use ||= @parent.mixins_in_use
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
