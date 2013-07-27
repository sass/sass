module Sass
  module Util
    module Test
      def skip(msg = nil, bt = caller)
        super if defined?(super)
        return
      end
    end
  end
end
