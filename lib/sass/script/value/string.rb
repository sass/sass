module Sass::Script::Value
  # A SassScript object representing a CSS string *or* a CSS identifier.
  class String < Base
    # The Ruby value of the string.
    #
    # @return [String]
    attr_reader :value

    # Whether this is a CSS string or a CSS identifier.
    # The difference is that strings are written with double-quotes,
    # while identifiers aren't.
    #
    # @return [Symbol] `:string` or `:identifier`
    attr_reader :type

    # Creates a new string.
    #
    # @param value [String] See \{#value}
    # @param type [Symbol] See \{#type}
    def initialize(value, type = :identifier)
      super(value)
      @type = type
    end

    # @see Value#plus
    def plus(other)
      other_str = other.is_a?(Sass::Script::Value::String) ? other.value : other.to_s
      Sass::Script::Value::String.new(value + other_str, type)
    end

    # @see Value#to_s
    def to_s(opts = {})
      if @type == :identifier
        return @value.gsub(/\n\s*/, " ")
      end

      return "\"#{value.gsub('"', "\\\"")}\"" if opts[:quote] == %q{"}
      return "'#{value.gsub("'", "\\'")}'" if opts[:quote] == %q{'}
      return "\"#{value}\"" unless value.include?('"')
      return "'#{value}'" unless value.include?("'")
      "\"#{value.gsub('"', "\\\"")}\"" # '
    end

    # @see Value#to_sass
    def to_sass(opts = {})
      to_s
    end
  end
end
