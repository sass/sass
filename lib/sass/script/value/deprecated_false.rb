module Sass::Script::Value
  # A SassScript object representing a `false` value that came from a call to
  # `index()`. It will print deprecation warnings if it's used with `==`.
  class DeprecatedFalse < Bool
    def self.new(environment)
      obj = allocate
      obj.send(:initialize, environment)
      obj
    end

    def initialize(environment)
      @value = false
      @global_env = environment.global_env
      if (frame = environment.stack.frames.last)
        @filename = frame.filename
        @line = frame.line
      end
    end

    def eq(other)
      if other.value == false && !warned?
        self.warned = true
        Sass::Util.sass_warn <<WARNING + @global_env.stack.to_s.gsub(/^/, '        ')
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "== false" on
the return value. For example, instead of "@if index(...) == false", just write
"@if not index(...)".
WARNING
      end
      Bool.new(other.value == false)
    end

    def neq(other)
      if other.value.nil? && !warned?
        self.warned = true
        Sass::Util.sass_warn <<WARNING + @global_env.stack.to_s.gsub(/^/, '        ')
DEPRECATION WARNING: The return value of index() will change from "false" to
"null" in future versions of Sass. For compatibility, avoid using "!= null" on
the return value.
WARNING
      end
      Bool.new(other.value != false)
    end

    private

    def warned?
      @global_env.deprecated_false_warning_given.include?([@filename, @line])
    end

    def warned=(value)
      @global_env.deprecated_false_warning_given << [@filename, @line]
    end
  end
end
