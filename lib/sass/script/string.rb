require 'sass/script/literal'

module Sass::Script
  # A SassScript object representing a string of text.
  class String < Literal
    # The Ruby value of the string.
    #
    # @return [String]
    attr_reader :value
    alias_method :to_s, :value

    # @see Node#to_sass
    def to_sass
      # Replace single backslashes with double. Really.
      value = self.value.gsub("\\", "\\\\\\\\")
      return "\"#{value}\"" unless value.include?('"')
      return "'#{value}'" unless value.include?("'")
      "\"#{value.gsub('"', "\\\"")}\"" #'
    end
  end
end
