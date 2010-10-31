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
      !Haml::Util.ruby1_8?
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      "#{'  ' * tabs}@charset \"#{name}\"#{semi fmt}\n"
    end

    # Computes the CSS for the directive.
    #
    # @param tabs [Fixnum] The level of indentation for the CSS
    # @return [String] The resulting CSS
    def _to_s(tabs)
      "@charset \"#{name}\";"
    end
  end
end
