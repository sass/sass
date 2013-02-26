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
      # @return [(Array<Script::Node>, {String => Script::Node}, Script::Node)]
      #   The root nodes of the positional arguments, keyword arguments, and
      #   splat argument. Keyword arguments are in a hash from names to values.
      # @raise [Sass::SyntaxError] if the argument list isn't valid SassScript
      def parse_mixin_include_arglist
        args, keywords = [], {}
        if try_tok(:lparen)
          args, keywords, splat = mixin_arglist || [[], {}]
          assert_tok(:rparen)
        end
        assert_done

        args.each {|a| a.options = @options}
        keywords.each {|k, v| v.options = @options}
        splat.options = @options if splat
        return args, keywords, splat
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses the argument list for a mixin definition.
      #
      # @return [(Array<Script::Node>, Script::Node)]
      #   The root nodes of the arguments, and the splat argument.
      # @raise [Sass::SyntaxError] if the argument list isn't valid SassScript
      def parse_mixin_definition_arglist
        args, splat = defn_arglist!(false)
        assert_done

        args.each do |k, v|
          k.options = @options
          v.options = @options if v
        end
        splat.options = @options if splat
        return args, splat
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parses the argument list for a function definition.
      #
      # @return [(Array<Script::Node>, Script::Node)]
      #   The root nodes of the arguments, and the splat argument.
      # @raise [Sass::SyntaxError] if the argument list isn't valid SassScript
      def parse_function_definition_arglist
        args, splat = defn_arglist!(true)
        assert_done

        args.each do |k, v|
          k.options = @options
          v.options = @options if v
        end
        splat.options = @options if splat
        return args, splat
      rescue Sass::SyntaxError => e
        e.modify_backtrace :line => @lexer.line, :filename => @options[:filename]
        raise e
      end

      # Parse a single string value, possibly containing interpolation.
      # Doesn't assert that the scanner is finished after parsing.
      #
      # @return [Script::Node] The root node of the parse tree.
      # @raise [Sass::SyntaxError] if the string isn't valid SassScript
      def parse_string
        unless (peek = @lexer.peek) &&
            (peek.type == :string ||
            (peek.type == :funcall && peek.value.downcase == 'url'))
          lexer.expected!("string")
        end

        expr = assert_expr :funcall
        expr.options = @options
        @lexer.unpeek!
        expr
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
        :comma, :single_eq, :space, :or, :and,
        [:eq, :neq],
        [:gt, :gte, :lt, :lte],
        [:plus, :minus],
        [:times, :div, :mod],
      ]

      ASSOCIATIVE = [:plus, :times]

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

        # Returns whether or not the given operation is associative.
        #
        # @private
        def associative?(op)
          ASSOCIATIVE.include?(op)
        end

        private

        # Defines a simple left-associative production.
        # name is the name of the production,
        # sub is the name of the production beneath it,
        # and ops is a list of operators for this precedence level
        def production(name, sub, *ops)
          class_eval <<RUBY, __FILE__, __LINE__ + 1
            def #{name}
              interp = try_ops_after_interp(#{ops.inspect}, #{name.inspect}) and return interp
              return unless e = #{sub}
              while tok = try_tok(#{ops.map {|o| o.inspect}.join(', ')})
                if interp = try_op_before_interp(tok, e)
                  return interp unless other_interp = try_ops_after_interp(#{ops.inspect}, #{name.inspect}, interp)
                  return other_interp
                end

                line = @lexer.line
                e = Operation.new(e, assert_expr(#{sub.inspect}), tok.type)
                e.line = line
              end
              e
            end
RUBY
        end

        def unary(op, sub)
          class_eval <<RUBY, __FILE__, __LINE__ + 1
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

      def expr
        interp = try_ops_after_interp([:comma], :expr) and return interp
        line = @lexer.line
        return unless e = interpolation
        arr = [e]
        while tok = try_tok(:comma)
          if interp = try_op_before_interp(tok, e)
            return interp unless other_interp = try_ops_after_interp([:comma], :expr, interp)
            return other_interp
          end
          arr << assert_expr(:interpolation)
        end
        arr.size == 1 ? arr.first : node(List.new(arr, :comma), line)
      end

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

      def try_ops_after_interp(ops, name, prev = nil)
        return unless @lexer.after_interpolation?
        return unless op = try_tok(*ops)
        interp = try_op_before_interp(op, prev) and return interp

        wa = @lexer.whitespace?
        str = Script::String.new(Lexer::OPERATORS_REVERSE[op.type])
        str.line = @lexer.line
        interp = Script::Interpolation.new(prev, str, assert_expr(name), !:wb, wa, :originally_text)
        interp.line = @lexer.line
        return interp
      end

      def interpolation(first = space)
        e = first
        while interp = try_tok(:begin_interpolation)
          wb = @lexer.whitespace?(interp)
          line = @lexer.line
          mid = parse_interpolated
          wa = @lexer.whitespace?
          e = Script::Interpolation.new(e, mid, space, wb, wa)
          e.line = line
        end
        e
      end

      def space
        line = @lexer.line
        return unless e = or_expr
        arr = [e]
        while e = or_expr
          arr << e
        end
        arr.size == 1 ? arr.first : node(List.new(arr, :space), line)
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
        if color = Color::COLOR_NAMES[name.value.downcase]
          return node(Color.new(color))
        end
        node(Script::String.new(name.value, :identifier))
      end

      def funcall
        return raw unless tok = try_tok(:funcall)
        args, keywords, splat = fn_arglist || [[], {}]
        assert_tok(:rparen)
        node(Script::Funcall.new(tok.value, args, keywords, splat))
      end

      def defn_arglist!(must_have_parens)
        if must_have_parens
          assert_tok(:lparen)
        else
          return [], nil unless try_tok(:lparen)
        end
        return [], nil if try_tok(:rparen)

        res = []
        splat = nil
        must_have_default = false
        loop do
          c = assert_tok(:const)
          var = Script::Variable.new(c.value)
          if try_tok(:colon)
            val = assert_expr(:space)
            must_have_default = true
          elsif must_have_default
            raise SyntaxError.new("Required argument #{var.inspect} must come before any optional arguments.")
          elsif try_tok(:splat)
            splat = var
            break
          end
          res << [var, val]
          break unless try_tok(:comma)
        end
        assert_tok(:rparen)
        return res, splat
      end

      def fn_arglist
        arglist(:equals, "function argument")
      end

      def mixin_arglist
        arglist(:interpolation, "mixin argument")
      end

      def arglist(subexpr, description)
        return unless e = send(subexpr)

        args = []
        keywords = {}
        loop do
          if @lexer.peek && @lexer.peek.type == :colon
            name = e
            @lexer.expected!("comma") unless name.is_a?(Variable)
            assert_tok(:colon)
            value = assert_expr(subexpr, description)

            if keywords[name.underscored_name]
              raise SyntaxError.new("Keyword argument \"#{name.to_sass}\" passed more than once")
            end

            keywords[name.underscored_name] = value
          else
            if !keywords.empty?
              raise SyntaxError.new("Positional arguments must come before keyword arguments.")
            end

            return args, keywords, e if try_tok(:splat)
            args << e
          end

          return args, keywords unless try_tok(:comma)
          e = assert_expr(subexpr, description)
        end
      end

      def raw
        return special_fun unless tok = try_tok(:raw)
        node(Script::String.new(tok.value))
      end

      def special_fun
        return paren unless tok = try_tok(:special_fun)
        first = node(Script::String.new(tok.value.first))
        Sass::Util.enum_slice(tok.value[1..-1], 2).inject(first) do |l, (i, r)|
          Script::Interpolation.new(
            l, i, r && node(Script::String.new(r)),
            false, false)
        end
      end

      def paren
        return variable unless try_tok(:lparen)
        was_in_parens = @in_parens
        @in_parens = true
        line = @lexer.line
        e = expr
        assert_tok(:rparen)
        return e || node(List.new([], :space), line)
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
        (t = try_tok(:color, :bool, :null)) && (return t.value)
      end

      # It would be possible to have unified #assert and #try methods,
      # but detecting the method/token difference turns out to be quite expensive.

      EXPR_NAMES = {
        :string => "string",
        :default => "expression (e.g. 1px, bold)",
        :mixin_arglist => "mixin argument",
        :fn_arglist => "function argument",
      }

      def assert_expr(name, expected = nil)
        (e = send(name)) && (return e)
        @lexer.expected!(expected || EXPR_NAMES[name] || EXPR_NAMES[:default])
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

      def node(node, line = @lexer.line)
        node.line = line
        node
      end
    end
  end
end
