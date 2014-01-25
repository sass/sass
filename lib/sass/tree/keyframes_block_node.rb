module Sass::Tree
  # A static node reprenting a rule within a `@keyframes` directive.
  #
  # @see Sass::Tree
  class KeyframesBlockNode < Node
    attr_accessor :value

    attr_accessor :resolved_value

    def initialize(value)
      @value = value
      super()
    end
  end
end
