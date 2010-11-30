require 'sass/tree/node'

module Sass::Tree
  # A static node representing a mixin include.
  # When in a static tree, the sole purpose is to wrap exceptions
  # to add the mixin to the backtrace.
  #
  # @see Sass::Tree
  class MixinNode < Node
    # The name of the mixin.
    # @return [String]
    attr_reader :name

    # The arguments to the mixin.
    # @return [Array<Script::Node>]
    attr_reader :args

    # A hash from keyword argument names to values.
    # @return [{String => Script::Node}]
    attr_reader :keywords

    # @see Node#options=
    def options=(opts)
      super
      @args.each {|a| a.context = :equals} if opts[:sass2]
      @keywords.each {|k, v| v.context = :equals} if opts[:sass2]
    end

    # @param name [String] The name of the mixin
    # @param args [Array<Script::Node>] See \{#args}
    # @param keywords [{String => Script::Node}] See \{#keywords}
    def initialize(name, args, keywords)
      @name = name
      @args = args
      @keywords = keywords
      super()
    end

    protected

    # @see Node#to_src
    def to_src(tabs, opts, fmt)
      unless @args.empty? && @keywords.empty?
        args = @args.map {|a| a.to_sass(opts)}.join(", ")
        keywords = @keywords.map {|k, v| "$#{dasherize(k, opts)}: #{v.to_sass(opts)}"}.join(', ')
        arglist = "(#{args}#{', ' unless args.empty? || keywords.empty?}#{keywords})"
      end
      "#{'  ' * tabs}#{fmt == :sass ? '+' : '@include '}#{dasherize(@name, opts)}#{arglist}#{semi fmt}\n"
    end
  end
end
