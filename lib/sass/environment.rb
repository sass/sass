require 'set'

module Sass
  # The abstract base class for lexical environments for SassScript.
  class Environment
    class << self
      # Note: when updating this,
      # update sass/yard/inherited_hash.rb as well.
      def inherited_hash_accessor(name)
        inherited_hash_reader(name)
        inherited_hash_writer(name)
      end

      def inherited_hash_reader(name)
        class_eval <<-RUBY, __FILE__, __LINE__ + 1
          def #{name}(name)
            _#{name}(name.tr('_', '-'))
          end

          def _#{name}(name)
            (@#{name}s && @#{name}s[name]) || @parent && @parent._#{name}(name)
          end
          protected :_#{name}

          def is_#{name}_global?(name)
            name = name.tr('_', '-')
            return !@parent if @#{name}s && @#{name}s.has_key?(name)
            @parent && @parent.is_#{name}_global?(name)
          end
        RUBY
      end

      def inherited_hash_writer(name)
        class_eval <<-RUBY, __FILE__, __LINE__ + 1
          def set_#{name}(name, value)
            name = name.tr('_', '-')
            @#{name}s[name] = value unless try_set_#{name}(name, value)
          end

          def try_set_#{name}(name, value)
            @#{name}s ||= {}
            if @#{name}s.include?(name)
              @#{name}s[name] = value
              true
            elsif @parent && !@parent.global?
              @parent.try_set_#{name}(name, value)
            else
              false
            end
          end
          protected :try_set_#{name}

          def set_local_#{name}(name, value)
            @#{name}s ||= {}
            @#{name}s[name.tr('_', '-')] = value
          end

          def set_global_#{name}(name, value)
            global_env.set_#{name}(name, value)
          end
        RUBY
      end
    end

    inherited_hash_reader :fn
    inherited_hash_writer :fn

    inherited_hash_reader :mx
    inherited_hash_writer :mx

    inherited_hash_reader :var
    inherited_hash_writer :var

    # @param options [{Symbol => Object}] The options hash. See
    #   {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    # @param parent [Environment] See \{#parent}
    def initialize(parent = nil)
      @parent = parent
      @ident_count = 0
      @idents = {}
    end

    def unique_ident(name = nil)
      return global_env.unique_ident(name) unless global?
      @ident_count += 1
      "_s_#{(name || 'i').to_s.gsub(/[^a-zA-Z0-9_]/, '_')}_#{@ident_count}"
    end

    def fn_variable(name)
      ident, function = fn(name)
      return unless ident
      ident = "@#{ident}" if is_fn_global?(name)
      return ident, function
    end

    def declare_fn(name, function)
      ident = set_local_fn(name, [unique_ident("fn_#{name}"), function]).first
      global? ? "@#{ident}" : ident
    end

    def mx_variable(name)
      ident, mixin = mx(name)
      return unless ident
      ident = "@#{ident}" if is_mx_global?(name)
      return ident, mixin
    end

    def declare_mx(name, mixin)
      ident = set_local_mx(name, [unique_ident("mx_#{name}"), mixin]).first
      global? ? "@#{ident}" : ident
    end

    def var_variable(name)
      # Global variables may be defined non-lexically and still be accessible.
      return "@" + Sass::Util.consistent_ident("var_#{name}") unless (ident = var(name))
      return is_var_global?(name) ? "@#{ident}" : ident
    end

    def declare_var(name)
      return set_local_var(name, unique_ident("var_#{name}")) unless global?
      ident = set_local_var(name, Sass::Util.consistent_ident("var_#{name}"))
      return "@#{ident}"
    end

    def assign_var(name)
      old_var_var = var_variable(name)
      return old_var_var if old_var_var && is_var_global?(name) == semi_global?
      declare_var(name)
    end

    def assign_global_var(name)
      return var_variable(name) if is_var_global?(name)
      global_env.declare_var(name)
    end

    # Returns whether this is the global environment.
    #
    # @return [Boolean]
    def global?
      @parent.nil?
    end

    def semi_global?
      global?
    end

    # The top-level Environment object.
    #
    # @return [Environment]
    def global_env
      @global_env ||= global? ? self : @parent.global_env
    end
  end

  # An environment that can write to in-scope global variables, but doesn't
  # create new variables in the global scope. Useful for top-level control
  # directives.
  class SemiGlobalEnvironment < Environment
    def semi_global?
      global? || @parent.semi_global?
    end
  end
end
