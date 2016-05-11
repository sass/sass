module Sass::Script::Value
  # A SassScript object representing a variable argument list. This works just
  # like a normal list, but can also contain keyword arguments.
  #
  # The keyword arguments attached to this list are unused except when this is
  # passed as a glob argument to a function or mixin.
  class ArgList < List
    # Whether \{#keywords} has been accessed. If so, we assume that all keywords
    # were valid for the function that created this ArgList.
    #
    # @return [Boolean]
    attr_accessor :keywords_accessed

    # Creates a new argument list.
    #
    # @param value [Array<Value>] See \{List#value}.
    # @param keywords [Hash<String, Value>, NormalizedMap<Value>] See \{#keywords}
    # @param separator [String] See \{List#separator}.
    def initialize(value, keywords, separator)
      super(value, separator: separator)
      if keywords.is_a?(Sass::Util::NormalizedMap)
        @keywords = keywords
      else
        @keywords = Sass::Util::NormalizedMap.new(keywords)
      end
    end

    # The keyword arguments attached to this list.
    #
    # @return [NormalizedMap<Value>]
    def keywords
      @keywords_accessed = true
      @keywords
    end

    # Remove the first positional argument from the ArgList and return
    # it along with a new arglist that is the remaining arguments
    #
    # @return [Value]
    def shift
      [value.first, ArgList.new(value[1..-1] || [], @keywords, separator)]
    end
  end
end
