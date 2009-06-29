module Sass::Script
  # The abstract superclass for SassScript objects.
  #
  # Many of these methods, especially the ones that correspond to SassScript operations,
  # are designed to be overridden by subclasses which may change the semantics somewhat.
  # The operations listed here are just the defaults.
  class Literal < Node
    require 'sass/script/string'
    require 'sass/script/number'
    require 'sass/script/color'
    require 'sass/script/bool'

    # Returns the Ruby value of the literal.
    # The type of this value varies based on the subclass.
    #
    # @return [Object]
    attr_reader :value

    # Creates a new literal.
    #
    # @param value [Object] The object for \{#value}
    def initialize(value = nil)
      @value = value
    end

    # Evaluates the literal.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] This literal
    def perform(environment)
      self
    end

    # The SassScript `and` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Literal] The result of a logical and:
    #   `other` if this literal isn't a false {Bool},
    #   and this literal otherwise
    def and(other)
      to_bool ? other : self
    end

    # The SassScript `or` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Literal] The result of the logical or:
    #   this literal if it isn't a false {Bool},
    #   and `other` otherwise
    def or(other)
      to_bool ? self : other
    end

    # The SassScript `==` operation.
    # **Note that this returns a {Sass::Script::Bool} object,
    # not a Ruby boolean**.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Bool] True if this literal is the same as the other,
    #   false otherwise
    def eq(other)
      Sass::Script::Bool.new(self.class == other.class && self.value == other.value)
    end

    # The SassScript `!=` operation.
    # **Note that this returns a {Sass::Script::Bool} object,
    # not a Ruby boolean**.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Bool] False if this literal is the same as the other,
    #   true otherwise
    def neq(other)
      Sass::Script::Bool.new(!eq(other).to_bool)
    end

    # The SassScript `==` operation.
    # **Note that this returns a {Sass::Script::Bool} object,
    # not a Ruby boolean**.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Bool] True if this literal is the same as the other,
    #   false otherwise
    def unary_not
      Sass::Script::Bool.new(!to_bool)
    end

    # The SassScript default operation (e.g. `!a !b`, `"foo" "bar"`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   separated by a space
    def concat(other)
      Sass::Script::String.new("#{self.to_s} #{other.to_s}")
    end

    # The SassScript `,` operation (e.g. `!a, !b`, `"foo", "bar"`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   separated by `", "`
    def comma(other)
      Sass::Script::String.new("#{self.to_s}, #{other.to_s}")
    end

    # The SassScript `+` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   without any separation
    def plus(other)
      Sass::Script::String.new(self.to_s + other.to_s)
    end

    # The SassScript `-` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   separated by `"-"`
    def minus(other)
      Sass::Script::String.new("#{self.to_s}-#{other.to_s}")
    end

    # The SassScript `/` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing both literals
    #   separated by `"/"`
    def div(other)
      Sass::Script::String.new("#{self.to_s}/#{other.to_s}")
    end

    # The SassScript unary `-` operation (e.g. `-!a`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing the literal
    #   preceded by `"-"`
    def unary_minus
      Sass::Script::String.new("-#{self.to_s}")
    end

    # The SassScript unary `/` operation (e.g. `/!a`).
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Script::String] A string containing the literal
    #   preceded by `"/"`
    def unary_div
      Sass::Script::String.new("/#{self.to_s}")
    end

    # @return [String] A readable representation of the literal
    def inspect
      value.inspect
    end

    # @return [Boolean] `true` (the Ruby boolean value)
    def to_bool
      true
    end

    # Compares this object with another.
    #
    # @param other [Object] The object to compare with
    # @return [Boolean] Whether or not this literal is equivalent to `other`
    def ==(other)
      eq(other).to_bool
    end

    # @return [Fixnum] The integer value of this literal
    # @raise [Sass::SyntaxError] if this literal isn't an integer
    def to_i
      raise Sass::SyntaxError.new("#{self.inspect} is not an integer.")
    end

    # @raise [Sass::SyntaxError] if this literal isn't an integer
    def assert_int!; to_i; end
  end
end
