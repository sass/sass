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

    # Returns the Sass/SCSS code for the media query list.
    #
    # @param options [{Symbol => Object}] An options hash (see {Sass::CSS#initialize}).
    # @return [String]
    def to_src(options)
      queries.map {|q| q.to_src(options)}.join(', ')
    end

    # Returns a deep copy of this query list and all its children.
    #
    # @return [QueryList]
    def deep_copy
      QueryList.new(queries.map {|q| q.deep_copy})
    end

    # Sets the options hash for the script nodes in the media query.
    #
    # @param options [{Symbol => Object}] The options has to set.
    def options=(options)
      queries.each {|q| q.options = options}
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
      t1 = t2 if t1.empty?
      t2 = t1 if t2.empty?
      if ((m1 == 'not') ^ (m2 == 'not'))
        return if t1 == t2
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
      q = Query.new([], [], other.expressions + expressions)
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

    # Returns the Sass/SCSS code for the media query.
    #
    # @param options [{Symbol => Object}] An options hash (see {Sass::CSS#initialize}).
    # @return [String]
    def to_src(options)
      src = ''
      src << Sass::Media._interp_to_src(modifier, options)
      src << ' ' unless modifier.empty?
      src << Sass::Media._interp_to_src(type, options)
      src << ' and ' unless type.empty? || expressions.empty?
      src << expressions.map {|e| e.to_src(options)}.join(' and ')
      src
    end

    # Returns a deep copy of this query and all its children.
    #
    # @return [Query]
    def deep_copy
      Query.new(
        modifier.map {|c| c.is_a?(Sass::Script::Node) ? c.deep_copy : c},
        type.map {|c| c.is_a?(Sass::Script::Node) ? c.deep_copy : c},
        expressions.map {|q| q.deep_copy})
    end

    # Sets the options hash for the script nodes in the media query.
    #
    # @param options [{Symbol => Object}] The options has to set.
    def options=(options)
      modifier.each {|m| m.options = options if m.is_a?(Sass::Script::Node)}
      type.each {|t| t.options = options if t.is_a?(Sass::Script::Node)}
      expressions.each {|e| e.options = options}
    end
  end

  # A media query expression.
  #
  #   '(' S* media_feature S* [ ':' S* expr ]? ')'
  class Expression
    # The name of the feature being queried for.
    #
    # @return [Sass::Script::Node]
    attr_accessor :name

    # The name of the feature after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_name

    # The value of the feature.
    #
    # @return [Sass::Script::Node]
    attr_accessor :value

    # The value of the feature after any SassScript has been resolved.
    # Only set once \{Tree::Visitors::Perform} has been run.
    #
    # @return [String]
    attr_accessor :resolved_value

    # @param name [Sass::Script::Node] See \{#name}
    # @param value [Sass::Script::Node] See \{#value}
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
      @resolved_name = yield name ? [name] : []
      @resolved_value = yield value ? [value] : []
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

    # Returns the Sass/SCSS code for the expression.
    #
    # @param options [{Symbol => Object}] An options hash (see {Sass::CSS#initialize}).
    # @return [String]
    def to_src(options)
      src = '('
      src << name.to_sass(options)
      src << ': ' << value.to_sass(options) if value
      src << ')'
      src
    end

    # Returns a deep copy of this expression.
    #
    # @return [Expression]
    def deep_copy
      Expression.new(name.deep_copy, value && value.deep_copy)
    end

    # Sets the options hash for the script nodes in the expression.
    #
    # @param options [{Symbol => Object}] The options has to set.
    def options=(options)
      name.options = options
      value.options = options if value
    end
  end

  # Converts an interpolation array to source.
  #
  # @param [Array<String, Sass::Script::Node>] The interpolation array to convert.
  # @param options [{Symbol => Object}] An options hash (see {Sass::CSS#initialize}).
  # @return [String]
  def self._interp_to_src(interp, options)
    interp.map do |r|
      next r if r.is_a?(String)
      "\#{#{r.to_sass(options)}}"
    end.join
  end
end
