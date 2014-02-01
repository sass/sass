module Sass
  module Util
    # Ruby 1.8 doesn't support an actual Random class with a settable seed.
    class CrossPlatformRandom
      def initialize(seed = nil)
        if Sass::Util.ruby1_8?
          srand(seed) if seed
        else
          @random = seed ? ::Random.new(seed) : ::Random.new
        end
      end

      def rand(*args)
        return @random.rand(*args) if @random
        Kernel.rand(*args)
      end
    end
  end
end
