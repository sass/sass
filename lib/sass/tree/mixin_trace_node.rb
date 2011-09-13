require 'sass/tree/node'

module Sass::Tree
  # A solely static node left over after a mixin include.
  # Its sole purpose is to wrap exceptions to add the mixin to the backtrace.
  #
  # @see Sass::Tree
  class MixinTraceNode < Node
    # The name of the mixin.
    # @return [String]
    attr_reader :name

    # @param name [String] The name of the mixin
    def initialize(name)
      @name = name
      self.has_children = true
      super()
    end

    # Initializes this node from a mixin node.
    # @param mixin [MixinNode]
    # @return [MixinTraceNode]
    def self.from_mixin(mixin)
      trace = new(mixin.name)
      trace.line = mixin.line
      trace.filename = mixin.filename
      trace.options = mixin.options
      trace
    end
  end
end
