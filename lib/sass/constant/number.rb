require 'sass/constant/literal'

module Sass::Constant
  class Number < Literal # :nodoc:

    attr_reader :numerator_units, :denominator_units

    PRECISION = 1000.0

    def parse(value)
      first, second, unit = value.scan(Literal::NUMBER)[0]
      @value = first.empty? ? second.to_i : "#{first}#{second}".to_f
      @numerator_units = unit.empty? ? [] : [unit]
      @denominator_units = []
    end

    def plus(other)
      if other.is_a? Number
        operate(other, :+)
      elsif other.is_a?(Color)
        other.plus(self)
      else
        Sass::Constant::String.from_value(self.to_s + other.to_s)
      end
    end

    def minus(other)
      if other.is_a? Number
        operate(other, :-)
      else
        raise NoMethodError.new(nil, :minus)
      end
    end

    def unary_minus
      Number.from_value(-value, numerator_units, denominator_units)
    end

    def times(other)
      if other.is_a? Number
        self.operate(other, :*)
      elsif other.is_a? Color
        other.times(self)
      else
        raise NoMethodError.new(nil, :times)
      end
    end

    def div(other)
      if other.is_a? Number
        operate(other, :/)
      else
        raise NoMethodError.new(nil, :div)
      end
    end

    def mod(other)
      if other.is_a?(Number)
        unless other.unitless?
          raise Sass::SyntaxError.new("Cannot modulo by a number with units: #{other.value}#{other.unit_repr}.")
        end
        operate(other, :%)
      else
        raise NoMethodError.new(nil, :mod)
      end
    end

    def equals(other)
      Sass::Constant::Bool.from_value(super.to_bool && self.unit_str == other.unit_str)
    end

    def to_s
      raise Sass::SyntaxError.new("Incompatible units: #{(numerator_units + denominator_units).join(" and ")}.") unless legal_units?
      value = @value
      value = if value % 1 == 0.0
        value.to_i
      else
        (value * PRECISION).round / PRECISION
      end
      "#{value}#{unit_str}"
    end

    def to_i
      super unless value % 1 == 0.0
      return value
    end

    def unitless?
      numerator_units.size == 0 && denominator_units.size == 0
    end

    def legal_units?
      (numerator_units.size == 0 || numerator_units.size == 1) && denominator_units.size == 0
    end

    protected

    def self.from_value(value, numerator_units = nil, denominator_units = nil)
      instance = super(value)
      instance.instance_variable_set('@numerator_units', Array(numerator_units))
      instance.instance_variable_set('@denominator_units', Array(denominator_units))
      instance.send(:normalize!)
      instance
    end

    def operate(other, operation)
      other.coerce!(operation, numerator_units, denominator_units)
      v = self.value.send(operation, other.value)
      Number.from_value(v, *compute_units(other, operation))
    rescue ArgumentError
      raise Sass::SyntaxError.new("Incompatible units: #{unit_repr} and #{other.unit_repr}.")      
    end

    def coerce!(operation, num_units, den_units)
      if [:+, :-].include?(operation)
        @value *= coercion_factor(numerator_units, num_units)
        @value /= coercion_factor(denominator_units, den_units)
        @numerator_units = num_units.dup
        @denominator_units = den_units.dup
      end
    end
    
    def coercion_factor(from_units, to_units)
      # get a list of unmatched units
      from_units = from_units.dup
      to_units = to_units.dup
      from_units.each_with_index do |u, i|
        if (j = to_units.index(u)) && to_units.delete_at(j)
          from_units.delete_at(i)
        end
      end
      if from_units.size != to_units.size
        raise ArgumentError.new
      elsif (from_units + to_units).any?{|u| CONVERTABLE_UNITS[u].nil?}
        raise ArgumentError.new
      else
        from_units.zip(to_units).inject(1) {|m,p| m * conversion_factor(p[0], p[1]) }
      end
    end

    def compute_units(other, operation)
      case operation
      when :*
        [numerator_units + other.numerator_units, denominator_units + other.denominator_units]
      when :/
        [numerator_units + other.denominator_units, denominator_units + other.numerator_units]
      else  
        [numerator_units, denominator_units]
      end
    end

    def unit_str
      rv = numerator_units.join(" * ")
      if denominator_units.any?
        rv << " / "
        rv << denominator_units.join(" * ")
      end
      rv
    end

    def unit_repr
      if unitless?
        "unitless"
      else
        unit_str
      end
    end

    def normalize!
      return if unitless?
      common_units = numerator_units - (numerator_units - denominator_units)
      @numerator_units -= common_units
      @denominator_units -= common_units
      @denominator_units.each_with_index do |d, i|
        if CONVERTABLE_UNITS[d] && (num = @numerator_units.detect{|n| CONVERTABLE_UNITS[n] })
          @value /= conversion_factor(d, num)
          @denominator_units.delete_at(i)
          @numerator_units.delete_at(@numerator_units.index(num))
        end
      end
    end

    CONVERTABLE_UNITS = {"mm" => 0,"cm" => 1,"in" => 2,"pt" => 3,"pc" => 4}
    #                     mm           cm            in                 pt          pc
    CONVERSION_TABLE = [[  1,           0.1,          0.0393700787,      2.83464567, 0.236220473  ], # mm
                        [ 10,           1,            0.3937007870,     28.3464567,  2.36220473   ], # cm
                        [ 25.4,         2.54,         1,                72,          6            ], # in
                        [ 0.352777778,  0.0352777778, 0.01388888888889,  1,          0.0833333333 ], # pt
                        [ 4.23333333,   0.423333333,  0.166666667,      12,          1            ]] # pc

    def conversion_factor(from_unit, to_unit)
      CONVERSION_TABLE[CONVERTABLE_UNITS[from_unit]][CONVERTABLE_UNITS[to_unit]]
    end
  end
end
