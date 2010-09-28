require 'sass/script/literal'

module Sass::Script
  # An abstract SassScript object representing a list.
  class List < Literal
    attr_accessor :elements
    def initialize(elements)
      Sass::Util.abstract(self)
    end
    
    def to_s
      @elements.map{|e| e.to_s}.join(delimeter)
    end

    # The SassScript `==` operation.
    # **Note that this returns a {Sass::Script::Bool} object,
    # not a Ruby boolean**.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Bool] True if this literal is the same as the other,
    #   false otherwise
    def eq(other)
      Sass::Script::Bool.new(
        self.class == other.class &&
        self.elements.size == other.elements.size &&
        self.elements.zip(other.elements).all?{|e1,e2| e1.eq(e2).to_bool}
      )
    end

    def to_bool
      self.elements.size > 0
    end

    # Don't raise an error
    def assert_list!; end

    # Returns a readable representation of this list.
    #
    # @return [String] The representation
    def inspect(opts = {})
      to_s
    end
    alias_method :to_sass, :inspect
  end

  class CommaList < List
    def initialize(elements)
      @elements = elements
    end

    def delimeter
      ", "
    end

    # The SassScript `,` operation (e.g. `$a, $b`, `"foo", "bar"`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   separated by `", "`
    def comma(other)
      Sass::Script::CommaList.new(elements + [other])
    end

  end

  class SpaceList < List
    def initialize(elements)
      @elements = elements
    end

    def delimeter
      " "
    end

    # The SassScript default operation (e.g. `$a $b`, `"foo" "bar"`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::SpaceList] 
    def concat(other)
      SpaceList.new(elements + [other])
    end

  end
end