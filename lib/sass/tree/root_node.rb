require 'sexp_processor'
require 'ruby2ruby'
require 'pp'

module Sass
  module Tree
    # A static node that is the root node of the Sass document.
    class RootNode < Node
      # The Sass template from which this node was created
      #
      # @param template [String]
      attr_reader :template

      # @param template [String] The Sass template from which this node was created
      def initialize(template)
        super()
        @template = template
      end

      # Runs the dynamic Sass code and computes the CSS for the tree.
      #
      # @return [String] The compiled CSS.
      def render
        css_tree.css
      end

      # Runs the dynamic Sass code and computes the CSS for the tree, along with
      # the sourcemap.
      #
      # @return [(String, Sass::Source::Map)] The compiled CSS, as well as
      #   the source map. @see #render
      def render_with_sourcemap
        css_tree.css_with_sourcemap
      end

      private

      def css_tree
        with_thread_options do
          Visitors::CheckNesting.visit(self)
          to_sexp = Visitors::ToSexp.new(options)
          sexp = to_sexp.visit(self)
          sexp = PrettifyProcessor.new.process(sexp)
          #pp sexp
          ruby = Sass::Ruby2Ruby.new.process(sexp)
          #puts ruby
          mapper = RubyMapper.new(ruby)

          environment = RuntimeEnvironment.new(@options)
          eval_context = Sass::Script::Functions::EvaluationContext.new(
            environment, mapper, to_sexp.fn_signatures, to_sexp.mx_signatures)
          environment.context = eval_context
          eval_context.instance_eval(ruby)
          begin
            result = eval_context._s_entrypoint(environment)
          rescue Sass::SyntaxError => e
            stack = mapper.stack_for(e.backtrace)
            e.sass_backtrace = stack.to_sass_backtrace
            raise e
          end
          result.options = options
          Visitors::CheckNesting.visit(result) # Check again to validate mixins
          result, extends = Visitors::Cssize.visit(result)
          Visitors::Extend.visit(result, extends)
          result
        end
      end

      # TODO: make this a Ruby2Ruby subclass, since each iteration
      # over the full sexp is pretty expensive.
      class PrettifyProcessor < SexpProcessor
        def initialize
          @last_line = 1
          super
        end

        def process_block(sexp)
          block = s(:block)
          sexp.shift
          process_block_body(block, sexp)
          block
        end

        def process_block_body(block, body)
          until body.empty?
            e = body.shift
            if !e.is_a?(Sexp)
              block << e
            elsif e.first == :comment
              text = e[1]
              next if text == "\n#-s- line: #{@last_line}"
              if text =~ /line: (\d+)/
                @last_line = $1.to_i
              end

              block << e
            elsif e.first == :block
              process_block_body(block, e[1..-1])
            else
              block << process(e)
            end
          end
        end

        def process_dstr(sexp)
          # Work around seattlerb/ruby2ruby#32
          dstr = s(:dstr)
          sexp.shift
          dstr << sexp.shift.dump[1...-1].gsub("\\\"", '"')
          while subsexp = sexp.shift
            if subsexp.first == :str || subsexp.first == :dsym
              dstr << s(subsexp.first, subsexp.last.dump[1...-1].gsub("\\\"", '"'))
            else
              dstr << process(subsexp)
            end
          end
          dstr
        end
      end
    end
  end
end
