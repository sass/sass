module Sass::Tree
  # A static node representing an unproccessed Sass `@charset` directive.
  #
  # @see Sass::Tree
  class CharsetNode < Node
    # The name of the charset.
    #
    # @return [String]
    attr_accessor :name

    # @param name [String] see \{#name}
    def initialize(name)
      @name = name
      super()
    end

    # @see Node#invisible?
    def invisible?
      !Sass::Util.ruby1_8?
    end
  end
end
