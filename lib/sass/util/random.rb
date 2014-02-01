# In Ruby 1.9+, there is a built-in Random class, which is globally defined.
# In case we are running on Ruby 1.8.7 (detected, by having no Random class defined),
# this method defines a mock Random class.
unless defined?(::Random)
  # Shim for ruby 1.8.7 support.
  class Random
    def initialize(seed = nil)
      srand(seed) if seed
    end

    def rand(*args)
      Kernel.rand(*args)
    end
  end
end
