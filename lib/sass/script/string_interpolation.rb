module Sass::Script
  class StringInterpolation < Node
    def initialize(before, mid, after)
      @before = before
      @mid = mid
      @after = after
    end

    def inspect
      "(string_interpolation #{@before.inspect} #{@mid.inspect} #{@after.inspect})"
    end

    def to_sass(opts = {})
      res = ""
      res << @before.to_sass(opts)[0...-1]
      res << '#{' << @mid.to_sass(opts) << '}'
      res << @after.to_sass(opts)[1..-1]
      res
    end

    def children
      [@before, @mid, @after].compact
    end

    protected

    def _perform(environment)
      res = ""
      res << @before.perform(environment).value
      val = @mid.perform(environment)
      res << (val.is_a?(Sass::Script::String) ? val.value : val.to_s)
      res << @after.perform(environment).value
      Sass::Script::String.new(res, :string)
    end
  end
end
