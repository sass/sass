module Sass::Script
  class Interpolation < Node
    def initialize(before, mid, after, wb, wa)
      @before = before
      @mid = mid
      @after = after
      @whitespace_before = wb
      @whitespace_after = wa
    end

    def inspect
      "(interpolation #{@before.inspect} #{@mid.inspect} #{after.inspect})"
    end

    def to_sass
      res = ""
      res << @before.to_sass if @before
      res << ' ' if @before && @whitespace_before
      res << '#{' << @mid.to_sass << '}'
      res << ' ' if @after && @whitespace_after
      res << @after.to_sass if @after
      res
    end

    def children
      [@before, @mid, @after].compact
    end

    protected

    def _perform(environment)
      res = ""
      res << @before.perform(environment).to_s if @before
      res << " " if @before && @whitespace_before
      val = @mid.perform(environment)
      res << (val.is_a?(Sass::Script::String) ? val.value : val.to_s)
      res << " " if @after && @whitespace_after
      res << @after.perform(environment).to_s if @after
      Sass::Script::String.new(res)
    end
  end
end
