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

    # Runs the SassScript in the media query list.
    #
    # @yield [interp] A block that should perform interpolation.
    # @yieldparam interp [Array<String, Sass::Script::Node>]
    #   An interpolation array to perform.
    # @yieldreturn [String] The interpolated value.
    def perform(&run_interp)
      @queries.each {|q| q.perform(&run_interp)}
    end

    # Merges this query list with another. The returned query list
    # queries for the intersection between the two inputs.
    #
    # Both query lists should be resolved.
    #
    # @param other [QueryList]
    # @return [QueryList?] The merged list, or nil if there is no intersection.
    def merge(other)
      new_queries = queries.map {|q1| other.queries.map {|q2| q1.merge(q2)}}.flatten.compact
      return if new_queries.empty?
      QueryList.new(new_queries)
    end

    # Returns the CSS for the media query list.
    #
    # @return [String]
    def to_css
      queries.map {|q| q.to_css}.join(', ')
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

    # The modifier for the query after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_modifier

    # The type of the query (e.g. `"screen"` or `"print"`).
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :type

    # The type of the query after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_type

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

    # Runs the SassScript in the media query.
    #
    # @yield [interp] A block that should perform interpolation.
    # @yieldparam interp [Array<String, Sass::Script::Node>]
    #   An interpolation array to perform.
    # @yieldreturn [String] The interpolated value.
    def perform(&run_interp)
      @resolved_modifier = yield modifier
      @resolved_type = yield type
      expressions.each {|e| e.perform(&run_interp)}
    end

    # Merges this query with another. The returned query queries for
    # the intersection between the two inputs.
    #
    # Both queries should be resolved.
    #
    # @param other [Query]
    # @return [Query?] The merged query, or nil if there is no intersection.
    def merge(other)
      m1, t1 = resolved_modifier.downcase, resolved_type.downcase
      m2, t2 = other.resolved_modifier.downcase, other.resolved_type.downcase
      if ((m1 == 'not') ^ (m2 == 'not')) && (t1 != t2)
        type = m1 == 'not' ? t2 : t1
        mod = m1 == 'not' ? m2 : m1
      elsif m1 == 'not' && m2 == 'not'
        # CSS has no way of representing "neither screen nor print"
        return unless t1 == t2
        type = t1
        mod = 'not'
      elsif t1 != t2
        return
      else # t1 == t2, neither m1 nor m2 are "not"
        type = t1
        mod = m1.empty? ? m2 : m1
      end
      q = Query.new([], [], expressions + other.expressions)
      q.resolved_type = type
      q.resolved_modifier = mod
      return q
    end

    # Returns the CSS for the media query.
    #
    # @return [String]
    def to_css
      css = ''
      css << resolved_modifier
      css << ' ' unless resolved_modifier.empty?
      css << resolved_type
      css << ' and ' unless resolved_type.empty? || expressions.empty?
      css << expressions.map {|e| e.to_css}.join(' and ')
      css
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

    # The name of the feature after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_name

    # The value of the feature.
    #
    # @return [Array<String, Sass::Script::Node>]
    attr_accessor :value

    # The value of the feature after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_value

    # @param name [Array<String, Sass::Script::Node>] See \{#name}
    # @param value [Array<String, Sass::Script::Node>] See \{#value}
    def initialize(name, value)
      @name = name
      @value = value
    end

    # Runs the SassScript in the expression.
    #
    # @yield [interp] A block that should perform interpolation.
    # @yieldparam interp [Array<String, Sass::Script::Node>]
    #   An interpolation array to perform.
    # @yieldreturn [String] The interpolated value.
    def perform
      @resolved_name = yield name
      @resolved_value = yield value
    end

    # Returns the CSS for the expression.
    #
    # @return [String]
    def to_css
      css = '('
      css << resolved_name
      css << ': ' << resolved_value unless resolved_value.empty?
      css << ')'
      css
    end
  end
end
