require 'haml/helpers'
require 'haml/buffer'
require 'haml/precompiler'
require 'haml/filters'
require 'haml/error'
require 'haml/util'

module Haml
  # This is the class where all the parsing and processing of the Haml
  # template is done. It can be directly used by the user by creating a
  # new instance and calling <tt>to_html</tt> to render the template. For example:
  #
  #   template = File.read('templates/really_cool_template.haml')
  #   haml_engine = Haml::Engine.new(template)
  #   output = haml_engine.to_html
  #   puts output
  class Engine
    include Precompiler

    # Allow reading and writing of the options hash
    attr :options, true

    # Creates a new instace of Haml::Engine that will compile the given
    # template string when <tt>render</tt> is called.
    # See README for available options.
    #
    #--
    # When adding options, remember to add information about them
    # to README!
    #++
    #
    def initialize(template, options = {})
      @options = {
        :suppress_eval => false,
        :attr_wrapper => "'",
        :locals => {},
        :autoclose => ['meta', 'img', 'link', 'br', 'hr', 'input', 'area'],
        :filters => {
          'sass' => Sass::Engine,
          'plain' => Haml::Filters::Plain,
          'preserve' => Haml::Filters::Preserve,
          'redcloth' => Haml::Filters::RedCloth,
          'textile' => Haml::Filters::Textile,
          'markdown' => Haml::Filters::Markdown }
      }

      @options.rec_merge! options

      unless @options[:suppress_eval]
        @options[:filters].merge!({
          'erb' => ERB,
          'ruby' => Haml::Filters::Ruby
        })
      end
      @options[:filters].rec_merge! options[:filters] if options[:filters]

      @template = template.strip #String
      @to_close_stack = []
      @output_tabs = 0
      @template_tabs = 0
      @index = 0
      @flat_spaces = -1

      precompile
    rescue Haml::Error => e
      e.add_backtrace_entry(@index, @options[:filename])
      raise e
    end

    # Processes the template and returns the result as a string.
    #
    # +scope+ is the context in which the template is evaluated.
    # If it's a Binding or Proc object,
    # Haml uses it as the second argument to Kernel#eval;
    # otherwise, Haml just uses its #instance_eval context.
    # Note that Haml modifies the context,
    # extending it with Haml::Helpers
    # and performing various other modifications.
    #
    # If a block is passed to render,
    # that block is run when +yield+ is called
    # within the template.
    #
    # Note that due to some Ruby quirks,
    # if scope is a Binding or Proc object and a block is given,
    # the evaluation context may not be quite what the user expects.
    # In particular, it's equivalent to passing <tt>eval("self", scope)</tt> as scope.
    # This won't have an effect in most cases,
    # but if you're relying on local variables defined in the context of scope,
    # they won't work.
    def render(scope = Object.new, &block)
      buffer = Haml::Buffer.new(options_for_buffer)

      if scope.is_a?(Binding) || scope.is_a?(Proc)
        scope_object = eval("self", scope)
        scope = scope_object.instance_eval{binding} if block_given?
      else
        scope_object = scope
        scope = scope_object.instance_eval{binding}
      end

      set_locals(@options[:locals].merge(:_hamlout => buffer, :_erbout => buffer.buffer), scope, scope_object)

      scope_object.instance_eval do
        extend Haml::Helpers
        @haml_stack ||= Array.new
        @haml_stack.push(buffer)
        @haml_is_haml = true
      end

      begin
        eval(@precompiled, scope, '(haml-eval)')
      rescue Exception => e
        raise add_exception_info(e, scope_object)
      end

      # Get rid of the current buffer
      scope_object.instance_eval do
        @haml_stack.pop
        @haml_is_haml = false
      end

      buffer.buffer
    end
    alias_method :to_html, :render

    # Returns a proc that, when called,
    # renders the template and returns the result as a string.
    #
    # +scope+ works the same as it does for render.
    #
    # The proc doesn't take a block;
    # any yields in the template will fail.
    def render_proc(scope = Object.new)
      if scope.is_a?(Binding) || scope.is_a?(Proc)
        scope_object = eval("self", scope)
      else
        scope_object = scope
        scope = scope_object.instance_eval{binding}
      end

      set_locals(@options[:locals], scope, scope_object)

      begin
        eval("proc {#{precompiled_with_ambles}}\n", scope, '(haml-eval)')
      rescue Exception => e
        raise add_exception_info(e, scope_object)
      end
    end

    private

    def set_locals(locals, scope, scope_object)
      scope_object.send(:instance_variable_set, '@_haml_locals', locals)
      set_locals = locals.keys.map { |k| "#{k} = @_haml_locals[#{k.inspect}]" }.join("\n")
      eval(set_locals, scope)
    end

    def add_exception_info(e, scope_object)
      metaclass = class << e; self; end
      metaclass.send(:include, Haml::Error)

      eval_line = e.backtrace[0].scan(/:([0-9]*)/)[0][0].to_i
      line_marker = @precompiled.split("\n")[0...eval_line].grep(/#haml_lineno: [0-9]+/)[-1]
      e.add_backtrace_entry(line_marker ? line_marker.scan(/[0-9]+/)[0].to_i : -1, @options[:filename])

      # Format Ruby compiler errors nicely
      message = e.message.scan(/compile error\n\(haml-eval\):[0-9]*: (.*)/)[0]
      metaclass.send(:define_method, :message) { "compile error: #{message}" } if message

      return e
    end

    # Returns a hash of options that Haml::Buffer cares about.
    # This should remain loadable form #inspect.
    def options_for_buffer
      {:attr_wrapper => @options[:attr_wrapper]}
    end
  end
end
