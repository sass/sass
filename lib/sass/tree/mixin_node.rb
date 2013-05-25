require 'sass/tree/node'

module Sass::Tree
  # A static node representing a mixin include.
  # When in a static tree, the sole purpose is to wrap exceptions
  # to add the mixin to the backtrace.
  #
  # @see Sass::Tree
  class MixinNode < Node
    # The name of the mixin.
    # @return [String]
    attr_reader :name

    # The arguments to the mixin.
    # @return [Array<Script::Tree::Node>]
    attr_accessor :args

    # A hash from keyword argument names to values.
    # @return [{String => Script::Tree::Node}]
    attr_accessor :keywords

    # The splat argument for this mixin, if one exists.
    #
    # @return [Script::Tree::Node?]
    attr_accessor :splat

    # @param name [String] The name of the mixin
    # @param args [Array<Script::Tree::Node>] See \{#args}
    # @param splat [Script::Tree::Node] See \{#splat}
    # @param keywords [{String => Script::Tree::Node}] See \{#keywords}
    def initialize(name, args, keywords, splat)
      @name = name
      @args = args
      @keywords = keywords
      @splat = splat
      super()
    end
  end
end
