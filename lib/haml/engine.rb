require 'haml/helpers'
require 'haml/buffer'
require 'haml/precompiler'
require 'haml/filters'
require 'haml/error'

module Haml
  # This is the frontend for using Haml programmatically.
  # It can be directly used by the user by creating a
  # new instance and calling \{#render} to render the template.
  # For example:
  #
  #     template = File.read('templates/really_cool_template.haml')
  #     haml_engine = Haml::Engine.new(template)
  #     output = haml_engine.render
  #     puts output
  class Engine
    include Precompiler

    # The options hash.
    # See {file:HAML_REFERENCE.md#haml_options the Haml options documentation}.
    #
    # @return [Hash<Symbol, Object>]
    attr_accessor :options

    # The indentation used in the Haml document,
    # or `nil` if the indentation is ambiguous
    # (for example, for a single-level document).
    #
    # @return [String]
    attr_accessor :indentation

    # @return [Boolean] Whether or not the format is XHTML.
    def xhtml?
      not html?
    end

    # @return [Boolean] Whether or not the format is any flavor of HTML.
    def html?
      html4? or html5?
    end

    # @return [Boolean] Whether or not the format is HTML4.
    def html4?
      @options[:format] == :html4
    end

    # @return [Boolean] Whether or not the format is HTML5.
    def html5?
      @options[:format] == :html5
    end

    # The source code that is evaluated to produce the Haml document.
    #
    # In Ruby 1.9, this is automatically converted to the correct encoding
    # (see {file:HAML_REFERENCE.md#encoding-option the `:encoding` option}).
    #
    # @return [String]
    def precompiled
      return @precompiled if ruby1_8?
      return @precompiled.encode(Encoding.find(@options[:encoding]))
    end

    # Precompiles the Haml template.
    #
    # @param template [String] The Haml template
    # @param options [Hash<Symbol, Object>] An options hash;
    #   see {file:HAML_REFERENCE.md#haml_options the Haml options documentation}
    # @raise [Haml::Error] if there's a Haml syntax error in the template
    def initialize(template, options = {})
      @options = {
        :suppress_eval => false,
        :attr_wrapper => "'",

        # Don't forget to update the docs in lib/haml.rb if you update these
        :autoclose => %w[meta img link br hr input area param col base],
        :preserve => %w[textarea pre],

        :filename => '(haml)',
        :line => 1,
        :ugly => false,
        :format => :xhtml,
        :escape_html => false,
      }
      unless ruby1_8?
        @options[:encoding] = Encoding.default_internal || "utf-8"
      end
      @options.merge! options
      @index = 0

      unless [:xhtml, :html4, :html5].include?(@options[:format])
        raise Haml::Error, "Invalid format #{@options[:format].inspect}"
      end

      if @options[:encoding] && @options[:encoding].is_a?(Encoding)
        @options[:encoding] = @options[:encoding].name
      end

      # :eod is a special end-of-document marker
      @template = (template.rstrip).split(/\r\n|\r|\n/) + [:eod, :eod]
      @template_index = 0
      @to_close_stack = []
      @output_tabs = 0
      @template_tabs = 0
      @flat = false
      @newlines = 0
      @precompiled = ''
      @to_merge = []
      @tab_change  = 0
      @temp_count = 0

      precompile
    rescue Haml::Error => e
      e.backtrace.unshift "#{@options[:filename]}:#{(e.line ? e.line + 1 : @index) + @options[:line] - 1}" if @index
      raise
    end

    # Processes the template and returns the result as a string.
    #
    # `scope` is the context in which the template is evaluated.
    # If it's a `Binding` or `Proc` object,
    # Haml uses it as the second argument to `Kernel#eval`;
    # otherwise, Haml just uses its `#instance_eval` context.
    #
    # Note that Haml modifies the evaluation context
    # (either the scope object or the `self` object of the scope binding).
    # It extends {Haml::Helpers}, and various instance variables are set
    # (all prefixed with `haml_`).
    # For example:
    #
    #     s = "foobar"
    #     Haml::Engine.new("%p= upcase").render(s) #=> "<p>FOOBAR</p>"
    #
    #     # s now extends Haml::Helpers
    #     s.responds_to?(:html_attrs) #=> true
    #
    # `locals` is a hash of local variables to make available to the template.
    # For example:
    #
    #     Haml::Engine.new("%p= foo").render(Object.new, :foo => "Hello, world!") #=> "<p>Hello, world!</p>"
    #
    # If a block is passed to render,
    # that block is run when `yield` is called
    # within the template.
    #
    # Due to some Ruby quirks,
    # if `scope` is a `Binding` or `Proc` object and a block is given,
    # the evaluation context may not be quite what the user expects.
    # In particular, it's equivalent to passing `eval("self", scope)` as `scope`.
    # This won't have an effect in most cases,
    # but if you're relying on local variables defined in the context of `scope`,
    # they won't work.
    #
    # @param scope [Binding, Proc, Object] The context in which the template is evaluated
    # @param locals [Hash<Symbol, Object>] Local variables that will be made available
    #   to the template
    # @param block [#to_proc] A block that can be yielded to within the template
    # @return [String] The rendered template
    def render(scope = Object.new, locals = {}, &block)
      buffer = Haml::Buffer.new(scope.instance_variable_get('@haml_buffer'), options_for_buffer)

      if scope.is_a?(Binding) || scope.is_a?(Proc)
        scope_object = eval("self", scope)
        scope = scope_object.instance_eval{binding} if block_given?
      else
        scope_object = scope
        scope = scope_object.instance_eval{binding}
      end

      set_locals(locals.merge(:_hamlout => buffer, :_erbout => buffer.buffer), scope, scope_object)

      scope_object.instance_eval do
        extend Haml::Helpers
        @haml_buffer = buffer
      end

      eval(precompiled, scope, @options[:filename], @options[:line])

      # Get rid of the current buffer
      scope_object.instance_eval do
        @haml_buffer = buffer.upper
      end

      buffer.buffer
    end
    alias_method :to_html, :render

    # Returns a proc that, when called,
    # renders the template and returns the result as a string.
    #
    # `scope` works the same as it does for render.
    #
    # The first argument of the returned proc is a hash of local variable names to values.
    # However, due to an unfortunate Ruby quirk,
    # the local variables which can be assigned must be pre-declared.
    # This is done with the `local_names` argument.
    # For example:
    #
    #     # This works
    #     Haml::Engine.new("%p= foo").render_proc(Object.new, :foo).call :foo => "Hello!"
    #       #=> "<p>Hello!</p>"
    #
    #     # This doesn't
    #     Haml::Engine.new("%p= foo").render_proc.call :foo => "Hello!"
    #       #=> NameError: undefined local variable or method `foo'
    #
    # The proc doesn't take a block; any yields in the template will fail.
    #
    # @param scope [Binding, Proc, Object] The context in which the template is evaluated
    # @param local_names [Array<Symbol>] The names of the locals that can be passed to the proc
    # @return [Proc] The proc that will run the template
    def render_proc(scope = Object.new, *local_names)
      if scope.is_a?(Binding) || scope.is_a?(Proc)
        scope_object = eval("self", scope)
      else
        scope_object = scope
        scope = scope_object.instance_eval{binding}
      end

      eval("Proc.new { |*_haml_locals| _haml_locals = _haml_locals[0] || {};" +
           precompiled_with_ambles(local_names) + "}\n", scope, @options[:filename], @options[:line])
    end

    # Defines a method on `object` with the given name
    # that renders the template and returns the result as a string.
    #
    # If `object` is a class or module,
    # the method will instead by defined as an instance method.
    # For example:
    #
    #     t = Time.now
    #     Haml::Engine.new("%p\n  Today's date is\n  .date= self.to_s").def_method(t, :render)
    #     t.render #=> "<p>\n  Today's date is\n  <div class='date'>Fri Nov 23 18:28:29 -0800 2007</div>\n</p>\n"
    #
    #     Haml::Engine.new(".upcased= upcase").def_method(String, :upcased_div)
    #     "foobar".upcased_div #=> "<div class='upcased'>FOOBAR</div>\n"
    #
    # The first argument of the defined method is a hash of local variable names to values.
    # However, due to an unfortunate Ruby quirk,
    # the local variables which can be assigned must be pre-declared.
    # This is done with the `local_names` argument.
    # For example:
    #
    #     # This works
    #     obj = Object.new
    #     Haml::Engine.new("%p= foo").def_method(obj, :render, :foo)
    #     obj.render(:foo => "Hello!") #=> "<p>Hello!</p>"
    #
    #     # This doesn't
    #     obj = Object.new
    #     Haml::Engine.new("%p= foo").def_method(obj, :render)
    #     obj.render(:foo => "Hello!") #=> NameError: undefined local variable or method `foo'
    #
    # Note that Haml modifies the evaluation context
    # (either the scope object or the `self` object of the scope binding).
    # It extends {Haml::Helpers}, and various instance variables are set
    # (all prefixed with `haml_`).
    #
    # @param object [Object, Module] The object on which to define the method
    # @param name [String, Symbol] The name of the method to define
    # @param local_names [Array<Symbol>] The names of the locals that can be passed to the proc
    def def_method(object, name, *local_names)
      method = object.is_a?(Module) ? :module_eval : :instance_eval

      object.send(method, "def #{name}(_haml_locals = {}); #{precompiled_with_ambles(local_names)}; end",
                  @options[:filename], @options[:line])
    end

    protected

    # Returns a subset of \{#options}: those that {Haml::Buffer} cares about.
    # All of the values here are such that when `#inspect` is called on the hash,
    # it can be `Kernel#eval`ed to get the same result back.
    #
    # See {file:HAML_REFERENCE.md#haml_options the Haml options documentation}.
    #
    # @return [Hash<Symbol, Object>] The options hash
    def options_for_buffer
      {
        :autoclose => @options[:autoclose],
        :preserve => @options[:preserve],
        :attr_wrapper => @options[:attr_wrapper],
        :ugly => @options[:ugly],
        :format => @options[:format],
        :encoding => @options[:encoding],
      }
    end

    private

    def set_locals(locals, scope, scope_object)
      scope_object.send(:instance_variable_set, '@_haml_locals', locals)
      set_locals = locals.keys.map { |k| "#{k} = @_haml_locals[#{k.inspect}]" }.join("\n")
      eval(set_locals, scope)
    end
  end
end
