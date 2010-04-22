module Sass::Script
  # The abstract superclass for SassScript parse tree nodes.
  #
  # Use \{#perform} to evaluate a parse tree.
  class Node
    # The options hash for this node.
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # The context in which this node was parsed,
    # which determines how some operations are performed.
    #
    # Can be `:equals`, which means it's part of a `$var = val` or `prop = val` assignment,
    # or `:default`, which means it's anywhere else
    # (including `$var: val` and `prop: val` assignments,
    # `#{}`-interpolations,
    # and other script contexts such as `@if` conditions).
    #
    # @return [Symbol]
    attr_reader :context

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
      children.each {|c| c.options = options}
    end

    # Sets the context for this node,
    # as well as for all child nodes.
    #
    # @param context [Symbol]
    # @see #context
    def context=(context)
      @context = context
      children.each {|c| c.context = context}
    end

    # Creates a new script node.
    def initialize
      @context = :default
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
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #children.")
    end

    # Returns the text of this SassScript expression.
    #
    # @return [String]
    def to_sass(opts = {})
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #to_sass.")
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
    #
    # @param environment [Sass::Environment] The environment in which to evaluate the SassScript
    # @return [Literal] The SassScript object that is the value of the SassScript
    # @see #perform
    def _perform(environment)
      raise NotImplementedError.new("All subclasses of Sass::Script::Node must override #_perform.")
    end
  end
end
