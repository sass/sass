require 'sass/script/lexer'

module Sass
  module Script
    # The parser for SassScript.
    # It parses a string of code into a tree of {Script::Node}s.
    class Parser
      # The line number of the parser's current position.
      #
      # @return [Fixnum]
      def line
        @lexer.line
      end

      # @param str [String, StringScanner] The source text to parse
      # @param line [Fixnum] The line on which the SassScript appears.
      #   Used for error reporting
      # @param offset [Fixnum] The number of characters in on which the SassScript appears.
      #   Used for error reporting
      # @param options [{Symbol => Object}] An options hash;
      #   see {file:SASS_REFERENCE.md#sass_options the Sass options documentation}
      def initialize(str, line, offset, options = {})
        @options = options
        @lexer = lexer_class.new(str, line, offset, options)
      end

      # Parses a SassScript expression within an interpolated segment (`#{}`).
      # This means that it stops when it comes across an unmatched `}`,
      # which signals the end of an interpolated segment,
      # it returns rather than throwing an error.
      #
      # @return [Script::Node] The root node of the parse tree
      # @raise [Sass::SyntaxError] if the expression isn't valid SassScript
      def parse_interpolated
        expr = assert_expr :expr
        assert_tok :end_interpolation
        expr.options = @options
        expr
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses a SassScript expression.
      #
      # @return [Script::Node] The root node of the parse tree
      # @raise [Sass::SyntaxError] if the expression isn't valid SassScript
      def parse
        expr = assert_expr :expr
        assert_done
        expr.options = @options
        expr
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses a SassScript expression,
      # ending it when it encounters one of the given identifier tokens.
      #
      # @param [#include?(String)] A set of strings that delimit the expression.
      # @return [Script::Node] The root node of the parse tree
      # @raise [Sass::SyntaxError] if the expression isn't valid SassScript
      def parse_until(tokens)
        @stop_at = tokens
        expr = assert_expr :expr
        assert_done
        expr.options = @options
        expr
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses the argument list for a mixin include.
      #
      # @return [Array<Script::Node>] The root nodes of the arguments.
      # @raise [Sass::SyntaxError] if the argument list isn't valid SassScript
      def parse_mixin_include_arglist
        args = []

        if try_tok(:lparen)
          args = arglist || args
          assert_tok(:rparen)
        end
        assert_done

        args.each {|a| a.options = @options}
        args
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses the argument list for a mixin definition.
      #
      # @return [Array<Script::Node>] The root nodes of the arguments.
      # @raise [Sass::SyntaxError] if the argument list isn't valid SassScript
      def parse_mixin_definition_arglist
        args = defn_arglist!(false)
        assert_done

        args.each do |k, v|
          k.options = @options
          v.options = @options if v
        end
        args
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses a SassScript expression.
      #
      # @overload parse(str, line, offset, filename = nil)
      # @return [Script::Node] The root node of the parse tree
      # @see Parser#initialize
      # @see Parser#parse
      def self.parse(*args)
        new(*args).parse
      end

      PRECEDENCE = [
        :comma, :single_eq, :concat, :or, :and,
        [:eq, :neq],
        [:gt, :gte, :lt, :lte],
        [:plus, :minus],
        [:times, :div, :mod],
      ]

      class << self
        # Returns an integer representing the precedence
        # of the given operator.
        # A lower integer indicates a looser binding.
        #
        # @private
        def precedence_of(op)
          PRECEDENCE.each_with_index do |e, i|
            return i if Array(e).include?(op)
          end
          raise "[BUG] Unknown operator #{op}"
        end

        private

        # Defines a simple left-associative production.
        # name is the name of the production,
        # sub is the name of the production beneath it,
        # and ops is a list of operators for this precedence level
        def production(name, sub, *ops)
          class_eval <<RUBY
            def #{name}
              interp = try_ops_after_interp(#{ops.inspect}, #{name.inspect}) and return interp
              return unless e = #{sub}
              while tok = try_tok(#{ops.map {|o| o.inspect}.join(', ')})
                interp = try_op_before_interp(tok, e) and return interp
                line = @lexer.line
                e = Operation.new(e, assert_expr(#{sub.inspect}), tok.type)
                e.line = line
              end
              e
            end
RUBY
        end

        def unary(op, sub)
          class_eval <<RUBY
            def unary_#{op}
              return #{sub} unless tok = try_tok(:#{op})
              interp = try_op_before_interp(tok) and return interp
              line = @lexer.line 
              op = UnaryOperation.new(assert_expr(:unary_#{op}), :#{op})
              op.line = line
              op
            end
RUBY
        end
      end

      private

      # @private
      def lexer_class; Lexer; end

      production :expr, :interpolation, :comma
      production :equals, :interpolation, :single_eq

      def try_op_before_interp(op, prev = nil)
        return unless @lexer.peek && @lexer.peek.type == :begin_interpolation
        wb = @lexer.whitespace?(op)
        str = Script::String.new(Lexer::OPERATORS_REVERSE[op.type])
        str.line = @lexer.line
        interp = Script::Interpolation.new(prev, str, nil, wb, !:wa, :originally_text)
        interp.line = @lexer.line
        interpolation(interp)
      end

      def try_ops_after_interp(ops, name)
        return unless @lexer.after_interpolation?
        return unless op = try_tok(*ops)
        interp = try_op_before_interp(op) and return interp

        wa = @lexer.whitespace?
        str = Script::String.new(Lexer::OPERATORS_REVERSE[op.type])
        str.line = @lexer.line
        interp = Script::Interpolation.new(nil, str, assert_expr(name), !:wb, wa, :originally_text)
        interp.line = @lexer.line
        return interp
      end

      def interpolation(first = concat)
        e = first
        while interp = try_tok(:begin_interpolation)
          wb = @lexer.whitespace?(interp)
          line = @lexer.line
          mid = parse_interpolated
          wa = @lexer.whitespace?
          e = Script::Interpolation.new(e, mid, concat, wb, wa)
          e.line = line
        end
        e
      end

      def concat
        return unless e = or_expr
        while sub = or_expr
          e = node(Operation.new(e, sub, :concat))
        end
        e
      end

      production :or_expr, :and_expr, :or
      production :and_expr, :eq_or_neq, :and
      production :eq_or_neq, :relational, :eq, :neq
      production :relational, :plus_or_minus, :gt, :gte, :lt, :lte
      production :plus_or_minus, :times_div_or_mod, :plus, :minus
      production :times_div_or_mod, :unary_plus, :times, :div, :mod

      unary :plus, :unary_minus
      unary :minus, :unary_div
      unary :div, :unary_not # For strings, so /foo/bar works
      unary :not, :ident

      def ident
        return funcall unless @lexer.peek && @lexer.peek.type == :ident
        return if @stop_at && @stop_at.include?(@lexer.peek.value)

        name = @lexer.next
        if color = Color::HTML4_COLORS[name.value]
          return node(Color.new(color))
        end
        node(Script::String.new(name.value, :identifier))
      end

      def funcall
        return raw unless tok = try_tok(:funcall)
        args = fn_arglist || []
        assert_tok(:rparen)
        node(Script::Funcall.new(tok.value, args))
      end

      def defn_arglist!(must_have_default)
        return [] unless try_tok(:lparen)
        return [] if try_tok(:rparen)
        res = []
        loop do
          line = @lexer.line
          offset = @lexer.offset + 1
          c = assert_tok(:const)
          var = Script::Variable.new(c.value)
          if tok = (try_tok(:colon) || try_tok(:single_eq))
            val = assert_expr(:concat)

            if tok.type == :single_eq
              val.context = :equals
              val.options = @options
              Script.equals_warning("mixin argument defaults", "$#{c.value}",
                val.to_sass, false, line, offset, @options[:filename])
            end
            must_have_default = true
          elsif must_have_default
            raise SyntaxError.new("Required argument #{var.inspect} must come before any optional arguments.")
          end
          res << [var, val]
          break unless try_tok(:comma)
        end
        assert_tok(:rparen)
        res
      end

      def fn_arglist
        return unless e = equals
        return [e] unless try_tok(:comma)
        [e, *assert_expr(:fn_arglist)]
      end

      def arglist
        return unless e = interpolation
        return [e] unless try_tok(:comma)
        [e, *assert_expr(:arglist)]
      end

      def raw
        return special_fun unless tok = try_tok(:raw)
        node(Script::String.new(tok.value))
      end

      def special_fun
        return paren unless tok = try_tok(:special_fun)
        first = node(Script::String.new(tok.value.first))
        Haml::Util.enum_slice(tok.value[1..-1], 2).inject(first) do |l, (i, r)|
          Script::Interpolation.new(
            l, i, r && node(Script::String.new(r)),
            false, false)
        end
      end

      def paren
        return variable unless try_tok(:lparen)
        was_in_parens = @in_parens
        @in_parens = true
        e = assert_expr(:expr)
        assert_tok(:rparen)
        return e
      ensure
        @in_parens = was_in_parens
      end

      def variable
        return string unless c = try_tok(:const)
        node(Variable.new(*c.value))
      end

      def string
        return number unless first = try_tok(:string)
        return first.value unless try_tok(:begin_interpolation)
        line = @lexer.line
        mid = parse_interpolated
        last = assert_expr(:string)
        interp = StringInterpolation.new(first.value, mid, last)
        interp.line = line
        interp
      end

      def number
        return literal unless tok = try_tok(:number)
        num = tok.value
        num.original = num.to_s unless @in_parens
        num
      end

      def literal
        (t = try_tok(:color, :bool)) && (return t.value)
      end

      # It would be possible to have unified #assert and #try methods,
      # but detecting the method/token difference turns out to be quite expensive.

      EXPR_NAMES = {
        :string => "string",
        :default => "expression (e.g. 1px, bold)",
        :arglist => "mixin argument",
        :fn_arglist => "function argument",
      }

      def assert_expr(name)
        (e = send(name)) && (return e)
        @lexer.expected!(EXPR_NAMES[name] || EXPR_NAMES[:default])
      end

      def assert_tok(*names)
        (t = try_tok(*names)) && (return t)
        @lexer.expected!(names.map {|tok| Lexer::TOKEN_NAMES[tok] || tok}.join(" or "))
      end

      def try_tok(*names)
        peeked =  @lexer.peek
        peeked && names.include?(peeked.type) && @lexer.next
      end

      def assert_done
        return if @lexer.done?
        @lexer.expected!(EXPR_NAMES[:default])
      end

      def node(node)
        node.line = @lexer.line
        node
      end
    end
  end
end
