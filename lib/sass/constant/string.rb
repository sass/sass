module Sass::Constant
  class String
    def initialize(value)
      @value = value
    end
    
    def to_s
      @value
    end
  end
end
