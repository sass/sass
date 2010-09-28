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

    # The SassScript `+` operation.
    #
    # Returns the concatenation two lists or adds
    # the element to the list on the side of the list where it is added.
    # That is:
    # $list1 + $list2 == concat($list1, $list2)
    # $list + $value == append($list1, $value)
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   without any separation
    def plus(other)
      if other.is_a?(Sass::Script::List)
        self.class.new(elements + other.elements)
      else
        self.class.new(elements + [other])
      end
    end

    # The SassScript `-` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A new list with `other` removed
    # from the list. If other is a list, all the elements in `other`
    # will be removed.
    def minus(other)
      other_elements = Array(other)
      self.class.new(elements.reject{|el| other_elements.include?(el)})
    end

    # The SassScript unary `+` operation (e.g. `+$list`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A new list containing the result of
    # calling unary plus on each element in the list.
    def unary_plus
      apply(:unary_plus)
    end

    # The SassScript unary `-` operation (e.g. `-$list`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A new list containing the result of
    # calling unary plus on each element in the list.
    def unary_minus
      apply(:unary_minus)
    end

    # The SassScript times `*` operation (e.g. `$list * 2`).
    #
    # When the right hand value is a unitless non-negative integer
    # The list will be duplicated that many times:
    #
    #     (1px 2px) * 2 == 1px 2px 1px 2px
    #
    # If you need to multiply a unitless number across the list,
    # perform the operation with the number on the left side
    #
    # Otherwise, the operation is applied to each element in the list:
    #
    #     (2 3) * 2px == 4px 6px
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A new list containing the result of
    # calling unary plus on each element in the list.
    def times(other)
      if other.is_a?(Sass::Script::Number) && other.unitless? && other.int? && other.to_i >= 0
        self.class.new(elements * other.to_i)
      else
        apply(:times, other)
      end
    end

    # The SassScript `/` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A new list containing the result of
    # calling `div(other)` on each element in the list.
    def div(other)
      apply(:div, other)
    end

    # Convert this object to a ruby boolean
    def to_bool
      elements.size > 0
    end

    # Convert this object to a ruby array.
    def to_a
      elements
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

    # Calls method on each element in the list with args passed to it.
    def apply(method, *args)
      self.class.new(elements.map{|el|
        el.send(method, *args)
      }.each{|el|
        el.context = context
      })
    end

    # Calls method on left_side for each element in the list with this
    # object as the first argument and any additional args passed to it.
    # @return a new list
    def right_apply(method, left_side, *args)
      self.class.new(elements.map{|el|
        left_side.send(method, *args.dup.unshift(el))
      }.each{|el|
        el.context = context
      })
    end
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