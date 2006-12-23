require 'sass/constant/literal'

module Sass::Constant
  class String
    include Literal
    
    def initialize(value)
      @value = value
    end
    
    def to_s
      @value
    end
  end
end
