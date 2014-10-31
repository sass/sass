module Sass
  # A subclass of Ruby2Ruby that handles the custom `:comment` node,
  # used to insert source-position comments in the generated Ruby code.
  #
  # @private
  class Ruby2Ruby < ::Ruby2Ruby
    def process_comment(exp)
      exp.shift.to_s
    end
  end
end
