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
    def to_s(opts = {})
      to_sass(opts)
    end

    # @param opts [{Symbol => Object}]
    #   `opts[:type]` -- The type of string to render this as.
    #     `:string`s have double quotes, `:identifier`s do not.
    #     Defaults to `:identifier`.
    # @see Node#to_sass
    def to_sass(opts = {})
      type = opts[:type] || self.type
      if type == :identifier
        if context == :equals && Sass::SCSS::RX.escape_ident(self.value).include?(?\\)
          return "unquote(#{Sass::Script::String.new(self.value, :string).to_sass})"
        elsif context == :equals && self.value.size == 0
          return %q{""}
        end
        return self.value
      end

      return "\"#{value.gsub('"', "\\\"")}\"" if opts[:quote] == %q{"}
      return "'#{value.gsub("'", "\\'")}'" if opts[:quote] == %q{'}
      return "\"#{value}\"" unless value.include?('"')
      return "'#{value}'" unless value.include?("'")
      "\"#{value.gsub('"', "\\\"")}\"" #'
    end
  end
end
