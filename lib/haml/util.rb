module Haml
  module Util
    class << self; include Haml::Util; end

    def to_hash(arr)
      arr.compact.inject({}) {|h, (k, v)| h[k] = v; h}
    end

    def map_keys(hash)
      to_hash(hash.map {|k, v| [yield(k), v]})
    end

    def map_vals(hash)
      to_hash(hash.map {|k, v| [k, yield(v)]})
    end

    def map_hash(hash, &block)
      to_hash(hash.map(&block))
    end
  end
end
