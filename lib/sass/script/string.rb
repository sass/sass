require 'sass/script/literal'

module Sass::Script
  class String < Literal # :nodoc:
    def to_s
      @value
    end
  end
end
