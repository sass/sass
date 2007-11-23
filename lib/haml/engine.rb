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
    def initialize(template, l_options = {})
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

      @options.rec_merge! l_options

      unless @options[:suppress_eval]
        @options[:filters].merge!({
          'erb' => ERB,
          'ruby' => Haml::Filters::Ruby
        })
      end
      @options[:filters].rec_merge! l_options[:filters] if l_options[:filters]

      @template = template.strip #String
      @to_close_stack = []
      @output_tabs = 0
      @template_tabs = 0
      @index = 0

      # This is the base tabulation of the currently active
      # flattened block. -1 signifies that there is no such block.
      @flat_spaces = -1

      # Only do the first round of pre-compiling if we really need to.
      precompile
    rescue Haml::Error => e
      e.add_backtrace_entry(@index, @options[:filename])
      raise e
    end

    # Processes the template and returns the result as a string.
    def render(scope = Object.new, &block)
      @buffer = Haml::Buffer.new(@options)
      compile scope, &block
      @buffer.buffer
    end
    alias_method :to_html, :render

    private

    # Takes <tt>@precompiled</tt>, a string buffer of Ruby code, and
    # evaluates it in the context of <tt>scope_object</tt>.
    # The code in <tt>@precompiled</tt> populates
    # <tt>@buffer</tt> with the compiled XHTML code.
    def compile(scope, &block)
      if Binding === scope
        scope_object = eval("self", scope)
      else
        scope_object = scope
        scope = scope.instance_eval{binding}
      end

      scope_object.extend Haml::Helpers
      buffer = @buffer
      scope_object.instance_eval do
        @haml_stack ||= Array.new
        @haml_stack.push(buffer)
      end

      scope_object.send(:instance_variable_set, '@_haml_locals', @options[:locals])
      set_locals = @options[:locals].keys.map { |k| "#{k} = @_haml_locals[#{k.inspect}]" }.join("\n")
      eval(set_locals, scope)

      begin
        eval(@precompiled, scope, '(haml-eval)', &block)
      rescue Exception => e
        raise add_exception_info(e, scope_object)
      end

      # Get rid of the current buffer
      scope_object.instance_eval do
        @haml_stack.pop
      end
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
  end
end
