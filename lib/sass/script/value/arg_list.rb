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
      super(value, separator)

      if keywords.is_a?(Sass::Util::NormalizedMap)
        @keywords = keywords
      else
        @keywords = Sass::Util::NormalizedMap.new(keywords)
      end
    end

    def merge(other)
      value = @value
      keywords = @keywords
      separator = self.separator

      if other.is_a?(ArgList)
        # [self] holds the explicit keyword arguments, which take
        # precedence over those in [other], which come from a
        # pre-existing ArgList.
        value += other.to_a
        keywords = other.keywords.merge(@keywords)
        separator = other.separator
      elsif other.is_a?(Map)
        keywords = Sass::Script::Helpers.arg_hash(other).merge(@keywords)
      elsif other.is_a?(List)
        value += other.to_a
        separator = other.separator
      else
        value += other.to_a
      end

      ArgList.new(value, keywords, separator)
    end

    # The keyword arguments attached to this list.
    #
    # @return [NormalizedMap<Value>]
    def keywords
      @keywords_accessed = true
      @keywords
    end

    # @private
    def keywords_safe
      @keywords
    end
  end
end
