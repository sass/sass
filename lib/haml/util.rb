module Haml
  module Util
    class << self; include Haml::Util; end

    RUBY_VERSION = ::RUBY_VERSION.split(".").map {|s| s.to_i}

    def ruby1_8?
      Haml::Util::RUBY_VERSION[0] == 1 && Haml::Util::RUBY_VERSION[1] < 9
    end

    def has?(attr, klass, method)
      klass.send("#{attr}s").include?(ruby1_8? ? method.to_s : method.to_sym)
    end

    def each_char(str, &block)
      if ruby1_8?
        str.each_byte(&block)
      else
        str.each_char(&block)
      end
    end
  end
end
