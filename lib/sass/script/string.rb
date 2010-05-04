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

    # In addition to setting the \{#context} of the string,
    # this sets the string to be an identifier if the context is `:equals`.
    #
    # @see Node#context=
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

    # @see Literal#plus
    def plus(other)
      other_str = other.is_a?(Sass::Script::String) ? other.value : other.to_s
      Sass::Script::String.new(self.value + other_str, self.type)
    end

    # @see Node#to_s
    def to_s(opts = {})
      if self.type == :identifier
        return %q{""} if context == :equals && self.value.size == 0
        return self.value.gsub("\n", " ")
      end

      return "\"#{value.gsub('"', "\\\"")}\"" if opts[:quote] == %q{"}
      return "'#{value.gsub("'", "\\'")}'" if opts[:quote] == %q{'}
      return "\"#{value}\"" unless value.include?('"')
      return "'#{value}'" unless value.include?("'")
      "\"#{value.gsub('"', "\\\"")}\"" #'
    end

    # @see Node#to_sass
    def to_sass(opts = {})
      if self.type == :identifier && context == :equals &&
          self.value !~ Sass::SCSS::RX::URI &&
          Sass::SCSS::RX.escape_ident(self.value).include?(?\\)
        return "unquote(#{Sass::Script::String.new(self.value, :string).to_sass})"
      else
        return to_s
      end
    end
  end
end
