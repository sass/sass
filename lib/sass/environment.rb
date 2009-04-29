module Sass
  class Environment
    attr_reader :parent
    attr_writer :options

    def initialize(parent = nil)
      @vars = {}
      @mixins = {}
      @parent = parent

      set_var("important", Script::String.new("!important")) unless @parent
    end

    def options
      @options || (parent && parent.options) || {}
    end

    def self.inherited_hash(name)
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
    inherited_hash :var
    inherited_hash :mixin
  end
end
