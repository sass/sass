require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a number.
  # SassScript numbers can have decimal values,
  # and can also have units.
  # For example, `12`, `1px`, and `10.45em`
  # are all valid values.
  #
  # Numbers can also have more complex units, such as `1px*em/in`.
  # These cannot be inputted directly in Sass code at the moment.
  class Number < Literal
    # The Ruby value of the number.
    #
    # @return [Numeric]
    attr_reader :value

    # A list of units in the numerator of the number.
    # For example, `1px*em/in*cm` would return `["px", "em"]`
    # @return [Array<String>] 
    attr_reader :numerator_units

    # A list of units in the denominator of the number.
    # For example, `1px*em/in*cm` would return `["in", "cm"]`
    # @return [Array<String>]
    attr_reader :denominator_units

    # The precision with which numbers will be printed to CSS files.
    # For example, if this is `1000.0`,
    # `3.1415926` will be printed as `3.142`.
    PRECISION = 1000.0

    # @param value [Numeric] The value of the number
    # @param numerator_units [Array<String>] See \{#numerator\_units}
    # @param denominator_units [Array<String>] See \{#denominator\_units}
    def initialize(value, numerator_units = [], denominator_units = [])
      super(value)
      @numerator_units = numerator_units
      @denominator_units = denominator_units
      normalize!
    end

    # The SassScript `+` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Adds the two numbers together, converting units if possible.
    #
    # {Color}
    # : Adds this number to each of the RGB color channels.
    #
    # {Literal}
    # : See {Literal#plus}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Literal] The result of the operation
    # @raise [Sass::UnitConversionError] if `other` is a number with incompatible units
    def plus(other)
      if other.is_a? Number
        operate(other, :+)
      elsif other.is_a?(Color)
        other.plus(self)
      else
        super
      end
    end

    # The SassScript binary `-` operation (e.g. `!a - !b`).
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Subtracts this number from the other, converting units if possible.
    #
    # {Literal}
    # : See {Literal#minus}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Literal] The result of the operation
    # @raise [Sass::UnitConversionError] if `other` is a number with incompatible units
    def minus(other)
      if other.is_a? Number
        operate(other, :-)
      else
        super
      end
    end

    # The SassScript unary `-` operation (e.g. `-!a`).
    #
    # @return [Number] The negative value of this number
    def unary_minus
      Number.new(-value, numerator_units, denominator_units)
    end

    # The SassScript `*` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Multiplies the two numbers together, converting units appropriately.
    #
    # {Color}
    # : Multiplies each of the RGB color channels by this number.
    #
    # @param other [Number, Color] The right-hand side of the operator
    # @return [Number, Color] The result of the operation
    # @raise [NoMethodError] if `other` is an invalid type
    def times(other)
      if other.is_a? Number
        self.operate(other, :*)
      elsif other.is_a? Color
        other.times(self)
      else
        raise NoMethodError.new(nil, :times)
      end
    end

    # The SassScript `/` operation.
    # Its functionality depends on the type of its argument:
    #
    # {Number}
    # : Divides this number by the other, converting units appropriately.
    #
    # {Literal}
    # : See {Literal#div}.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Literal] The result of the operation
    def div(other)
      if other.is_a? Number
        operate(other, :/)
      else
        super
      end
    end

    # The SassScript `%` operation.
    #
    # @param other [Number] The right-hand side of the operator
    # @return [Number] This number modulo the other
    # @raise [NoMethodError] if `other` is an invalid type
    # @raise [Sass::UnitConversionError] if `other` has any units
    def mod(other)
      if other.is_a?(Number)
        unless other.unitless?
          raise Sass::UnitConversionError.new("Cannot modulo by a number with units: #{other.inspect}.")
        end
        operate(other, :%)
      else
        raise NoMethodError.new(nil, :mod)
      end
    end

    # The SassScript `==` operation.
    #
    # @param other [Literal] The right-hand side of the operator
    # @return [Boolean] Whether this number is equal to the other object
    def eq(other)
      return Sass::Script::Bool.new(false) unless other.is_a?(Sass::Script::Number)
      this = self
      begin
        if unitless?
          this = this.coerce(other.numerator_units, other.denominator_units)
        else
          other = other.coerce(numerator_units, denominator_units)
        end
      rescue Sass::UnitConversionError
        return Sass::Script::Bool.new(false)
      end

      Sass::Script::Bool.new(this.value == other.value)
    end

    # The SassScript `>` operation.
    #
    # @param other [Number] The right-hand side of the operator
    # @return [Boolean] Whether this number is greater than the other
    # @raise [NoMethodError] if `other` is an invalid type
    def gt(other)
      raise NoMethodError.new(nil, :gt) unless other.is_a?(Number)
      operate(other, :>)
    end

    # The SassScript `>=` operation.
    #
    # @param other [Number] The right-hand side of the operator
    # @return [Boolean] Whether this number is greater than or equal to the other
    # @raise [NoMethodError] if `other` is an invalid type
    def gte(other)
      raise NoMethodError.new(nil, :gte) unless other.is_a?(Number)
      operate(other, :>=)
    end

    # The SassScript `<` operation.
    #
    # @param other [Number] The right-hand side of the operator
    # @return [Boolean] Whether this number is less than the other
    # @raise [NoMethodError] if `other` is an invalid type
    def lt(other)
      raise NoMethodError.new(nil, :lt) unless other.is_a?(Number)
      operate(other, :<)
    end

    # The SassScript `<=` operation.
    #
    # @param other [Number] The right-hand side of the operator
    # @return [Boolean] Whether this number is less than or equal to the other
    # @raise [NoMethodError] if `other` is an invalid type
    def lte(other)
      raise NoMethodError.new(nil, :lte) unless other.is_a?(Number)
      operate(other, :<=)
    end

    # @return [String] The CSS representation of this number
    # @raise [Sass::SyntaxError] if this number has units that can't be used in CSS
    #   (e.g. `px*in`)
    def to_s
      raise Sass::SyntaxError.new("#{inspect} isn't a valid CSS value.") unless legal_units?
      inspect
    end

    # Returns a readable representation of this number.
    #
    # This representation is valid CSS (and valid SassScript)
    # as long as there is only one unit.
    #
    # @return [String] The representation
    def inspect
      value =
        if self.value.is_a?(Float) && (self.value.infinite? || self.value.nan?)
          self.value
        elsif int?
          self.value.to_i
        else
          (self.value * PRECISION).round / PRECISION
        end
      "#{value}#{unit_str}"
    end

    # @return [Fixnum] The integer value of the number
    # @raise [Sass::SyntaxError] if the number isn't an integer
    def to_i
      super unless int?
      return value
    end

    # @return [Boolean] Whether or not this number is an integer.
    def int?
      value % 1 == 0.0
    end

    # @return [Boolean] Whether or not this number has no units.
    def unitless?
      numerator_units.empty? && denominator_units.empty?
    end

    # @return [Boolean] Whether or not this number has units that can be represented in CSS
    #   (that is, zero or one \{#numerator\_units}).
    def legal_units?
      (numerator_units.empty? || numerator_units.size == 1) && denominator_units.empty?
    end

    # Returns this number converted to other units.
    # The conversion takes into account the relationship between e.g. mm and cm,
    # as well as between e.g. in and cm.
    #
    # If this number has no units, it will simply return itself
    # with the given units.
    #
    # An incompatible coercion, e.g. between px and cm, will raise an error.
    #
    # @param num_units [Array<String>] The numerator units to coerce this number into.
    #   See {\#numerator\_units}
    # @param den_units [Array<String>] The denominator units to coerce this number into.
    #   See {\#denominator\_units}
    # @return [Number] The number with the new units
    # @raise [Sass::UnitConversionError] if the given units are incompatible with the number's
    #   current units
    def coerce(num_units, den_units)
      Number.new(if unitless?
                   self.value
                 else
                   self.value * coercion_factor(self.numerator_units, num_units) /
                     coercion_factor(self.denominator_units, den_units)
                 end, num_units, den_units)
    end

    protected

    def operate(other, operation)
      this = self
      if [:+, :-, :<=, :<, :>, :>=].include?(operation)
        if unitless?
          this = this.coerce(other.numerator_units, other.denominator_units)
        else
          other = other.coerce(numerator_units, denominator_units)
        end
      end
      # avoid integer division
      value = (:/ == operation) ? this.value.to_f : this.value
      result = value.send(operation, other.value)

      if result.is_a?(Numeric)
        Number.new(result, *compute_units(this, other, operation))
      else # Boolean op
        Bool.new(result)
      end
    end

    def coercion_factor(from_units, to_units)
      # get a list of unmatched units
      from_units, to_units = sans_common_units(from_units, to_units)

      if from_units.size != to_units.size || !convertable?(from_units | to_units)
        raise Sass::UnitConversionError.new("Incompatible units: '#{from_units.join('*')}' and '#{to_units.join('*')}'.")
      end

      from_units.zip(to_units).inject(1) {|m,p| m * conversion_factor(p[0], p[1]) }
    end

    def compute_units(this, other, operation)
      case operation
      when :*
        [this.numerator_units + other.numerator_units, this.denominator_units + other.denominator_units]
      when :/
        [this.numerator_units + other.denominator_units, this.denominator_units + other.numerator_units]
      else  
        [this.numerator_units, this.denominator_units]
      end
    end

    def unit_str
      rv = numerator_units.join("*")
      if denominator_units.any?
        rv << "/"
        rv << denominator_units.join("*")
      end
      rv
    end

    def normalize!
      return if unitless?
      @numerator_units, @denominator_units = sans_common_units(numerator_units, denominator_units)

      @denominator_units.each_with_index do |d, i|
        if convertable?(d) && (u = @numerator_units.detect(&method(:convertable?)))
          @value /= conversion_factor(d, u)
          @denominator_units.delete_at(i)
          @numerator_units.delete_at(@numerator_units.index(u))
        end
      end
    end

    # A hash of unit names to their index in the conversion table
    # @private
    CONVERTABLE_UNITS = {"in" => 0,        "cm" => 1,    "pc" => 2,    "mm" => 3,   "pt" => 4}
    # @private
    CONVERSION_TABLE = [[ 1,                2.54,         6,            25.4,        72        ], # in
                        [ nil,              1,            2.36220473,   10,          28.3464567], # cm
                        [ nil,              nil,          1,            4.23333333,  12        ], # pc
                        [ nil,              nil,          nil,          1,           2.83464567], # mm
                        [ nil,              nil,          nil,          nil,         1         ]] # pt

    def conversion_factor(from_unit, to_unit)
      res = CONVERSION_TABLE[CONVERTABLE_UNITS[from_unit]][CONVERTABLE_UNITS[to_unit]]
      return 1.0 / conversion_factor(to_unit, from_unit) if res.nil?
      res
    end

    def convertable?(units)
      Array(units).all?(&CONVERTABLE_UNITS.method(:include?))
    end

    def sans_common_units(units1, units2)
      units2 = units2.dup
      # Can't just use -, because we want px*px to coerce properly to px*mm
      return units1.map do |u|
        next u unless j = units2.index(u)
        units2.delete_at(j)
        nil
      end.compact, units2
    end
  end
end
