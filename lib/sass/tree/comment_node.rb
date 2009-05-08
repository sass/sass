require 'sass/tree/node'

module Sass::Tree
  class CommentNode < Node
    attr_accessor :lines
    attr_accessor :value
    attr_accessor :silent

    def initialize(value, silent)
      @lines = []
      @value = value[2..-1].strip
      @silent = silent
      super()
    end

    def ==(other)
      self.class == other.class && value == other.value && silent == other.silent && lines == other.lines
    end

    def to_s(tabs = 0, parent_name = nil)
      return if invisible?

      spaces = '  ' * (tabs - 1)
      spaces + "/* " + ([value] + lines.map {|l| l.text}).
        map{|l| l.sub(%r{ ?\*/ *$},'')}.join(style == :compact ? ' ' : "\n#{spaces} * ") + " */"
    end

    def invisible?
      style == :compressed || @silent
    end

    protected

    def _perform(environment)
      return [] if @silent
      self
    end
  end
end
