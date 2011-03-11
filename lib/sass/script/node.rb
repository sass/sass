module Sass::Script
  # The abstract superclass for SassScript parse tree nodes.
  #
  # Use \{#perform} to evaluate a parse tree.
  class Node
    # The options hash for this node.
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # The line of the document on which this node appeared.
    #
    # @return [Fixnum]
    attr_accessor :line

    # Sets the options hash for this node,
    # as well as for all child nodes.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @param options [{Symbol => Object}] The options
    def options=(options)
      @options = options
      children.each do |c|
        if c.is_a? Hash
          c.values.each {|v| v.options = options }
        else
          c.options = options
        end
      end
    end

    # Evaluates the node.
    #
    # \{#perform} shouldn't be overridden directly;
    # instead, override \{#\_perform}.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the SassScript
    def perform(environment)
      _perform(environment)
    rescue Sass::SyntaxError => e
      e.modify_backtrace(:line => line)
      raise e
    end

    # Returns all child nodes of this node.
    #
    # @return [Array<Node>]
    def children
      Sass::Util.abstract(self)
    end

    # Returns the text of this SassScript expression.
    #
    # @return [String]
    def to_sass(opts = {})
      Sass::Util.abstract(self)
    end

    protected

    # Converts underscores to dashes if the :dasherize option is set.
    def dasherize(s, opts)
      if opts[:dasherize]
        s.gsub(/_/,'-')
      else
        s
      end
    end

    # Evaluates this node.
    # Note that all {Literal} objects created within this method
    # should have their \{#options} attribute set, probably via \{#opts}.
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the SassScript
    # @see #perform
    def _perform(environment)
      Sass::Util.abstract(self)
    end

    # Sets the \{#options} field on the given literal and returns it
    #
    # @param literal [Literal]
    # @return [Literal]
    def opts(literal)
      literal.options = options
      literal
    end
  end
end
