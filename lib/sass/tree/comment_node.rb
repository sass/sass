require 'sass/tree/node'

module Sass::Tree
  class CommentNode < Node
    attr_accessor :lines
    attr_accessor :value

    def initialize(value, options)
      @lines = []
      @value = value[2..-1].strip
      super(options)
    end

    def ==(other)
      self.class == other.class && value == other.value && lines == other.lines
    end

    def silent?
      !!@options[:silent]
    end

    def to_s(tabs = 0, parent_name = nil)
      return if (@style == :compressed || silent?)

      spaces = '  ' * (tabs - 1)
      spaces + "/* " + ([value] + lines.map {|l| l.text}).
        map{|l| l.sub(%r{ ?\*/ *$},'')}.join(@style == :compact ? ' ' : "\n#{spaces} * ") + " */"
    end
  end
end
