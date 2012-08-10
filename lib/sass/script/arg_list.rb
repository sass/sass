module Sass::Script
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
    # @param value [Array<Literal>] See \{List#value}.
    # @param keywords [Hash<String, Literal>] See \{#keywords}
    # @param separator [String] See \{List#separator}.
    def initialize(value, keywords, separator)
      super(value, separator)
      @keywords = keywords
    end

    # The keyword arguments attached to this list.
    #
    # @return [Hash<String, Literal>]
    def keywords
      @keywords_accessed = true
      @keywords
    end

    # @see Node#children
    def children
      super + @keywords.values
    end

    # @see Node#deep_copy
    def deep_copy
      node = super
      node.instance_variable_set('@keywords',
        Sass::Util.map_hash(@keywords) {|k, v| [k, v.deep_copy]})
      node
    end

    protected

    # @see Node#_perform
    def _perform(environment)
      self
    end
  end
end
