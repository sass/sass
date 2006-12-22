module Sass::Constant
  class String
    def initialize(value)
      @value = value
    end
    
    def to_s(constants={})
      to_return = @value
      if @value[0] == Sass::Engine::CONSTANT_CHAR
        to_return = constants[@value[1..-1]]
        raise "Undefined constant:\n#{to_return}" unless to_return
      end
      to_return
    end
  end
end
