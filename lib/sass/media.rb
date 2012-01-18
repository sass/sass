# A namespace for the `@media` query parse tree.
module Sass::Media
  # A comma-separated list of queries.
  #
  #   media_query [ ',' S* media_query ]*
  class QueryList
    # The queries contained in this list.
    #
    # @return [Array<Query>]
    attr_accessor :queries

    # @param queries [Array<Query>] See \{#queries}
    def initialize(queries)
      @queries = queries
    end
  end

  # A single media query.
  #
  #   [ [ONLY | NOT]? S* media_type S* | expression ] [ AND S* expression ]*
  class Query
    # The modifier for the query.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :modifier

    # The type of the query (e.g. `"screen"` or `"print"`).
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :type

    # The trailing expressions in the query.
    #
    # @return [Array<Expression>]
    attr_accessor :expressions

    # @param modifier [Array<String, Sass::Script::Node>] See \{#modifier}
    # @param type [Array<String, Sass::Script::Node>] See \{#type}
    # @param expressions [Array<Expression>] See \{#expressions}
    def initialize(modifier, type, expressions)
      @modifier = modifier
      @type = type
      @expressions = expressions
    end
  end

  # A media query expression.
  #
  #   '(' S* media_feature S* [ ':' S* expr ]? ')'
  class Expression
    # The name of the feature being queried for.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :name

    # The value of the feature.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :value

    # @param name [Array<String, Sass::Script::Node>] See \{#name}
    # @param value [Array<String, Sass::Script::Node>] See \{#value}
    def initialize(name, value)
      @name = name
      @value = value
    end
  end
end
