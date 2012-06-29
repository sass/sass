module Sass::Tree
  # A node representing an `@import` rule that's importing plain CSS.
  #
  # @see Sass::Tree
  class CssImportNode < DirectiveNode
    # The URI being imported, either as a plain string or an interpolated
    # script string.
    #
    # @return [String, Sass::Script::Node]
    attr_accessor :uri

    # The text of the URI being imported after any interpolated SassScript has
    # been resolved. Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_uri

    # The media query for this rule, interspersed with {Sass::Script::Node}s
    # representing `#{}`-interpolation. Any adjacent strings will be merged
    # together.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :query

    # The media query for this rule, without any unresolved interpolation. It's
    # only set once {Tree::Node#perform} has been called.
    #
    # @return [Sass::Media::QueryList]
    attr_accessor :resolved_query

    # @param uri [String, Sass::Script::Node] See \{#uri}
    # @param query [Array<String, Sass::Script::Node>] See \{#query}
    def initialize(uri, query = nil)
      @uri = uri
      @query = query
      super('')
    end

    # @param uri [String] See \{#resolved_uri}
    # @return [CssImportNode]
    def self.resolved(uri)
      node = new(uri)
      node.resolved_uri = uri
      node
    end

    # @see DirectiveNode#value
    def value; raise NotImplementedError; end

    # @see DirectiveNode#resolved_value
    def resolved_value
      @resolved_value ||=
        begin
          str = "@import #{resolved_uri}"
          str << " #{resolved_query.to_css}" if resolved_query
          str
        end
    end
  end
end
