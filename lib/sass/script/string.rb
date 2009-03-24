require 'sass/script/literal'

module Sass::Script
  class String < Literal # :nodoc:
    INTERPOLATION = /(^|[^\\])\#\{([^}]*)\}/
    #TODO pass line & char context to perform
    def perform(environment)
      interpolated = @value.gsub(INTERPOLATION) do |match|
        "#{$1}#{Sass::Script.resolve($2, 0, 0, environment)}"
      end
      Sass::Script::String.new(interpolated)
    end

    def to_s
      @value
    end
  end
end
