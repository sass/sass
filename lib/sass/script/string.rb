require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a CSS string *or* a CSS identifier.
  class String < Literal
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

    def context=(context)
      super
      @type = :identifier if context == :equals
    end

    # Creates a new string.
    #
    # @param value [String] See \{#value}
    # @param type [Symbol] See \{#type}
    def initialize(value, type = :identifier)
      super(value)
      @type = type
    end

    def plus(other)
      other_str = other.is_a?(Sass::Script::String) ? other.value : other.to_s
      Sass::Script::String.new(self.value + other_str, self.type)
    end

    # @see Node#to_s
    def to_s
      to_sass
    end

    # @param type [Symbol] The type of string to render this as.
    #   `:string`s have double quotes, `:identifier`s do not.
    # @see Node#to_sass
    def to_sass(type = self.type)
      if type == :identifier
        if context == :equals && Sass::SCSS::RX.escape_ident(self.value).include?(?\\)
          return "unquote(#{Sass::Script::String.new(self.value, :string).to_sass})"
        end
        return self.value
      end

      # Replace single backslashes with double. Really.
      value = self.value.gsub("\\", "\\\\\\\\")
      return "\"#{value}\"" unless value.include?('"')
      return "'#{value}'" unless value.include?("'")
      "\"#{value.gsub('"', "\\\"")}\"" #'
    end
  end
end
