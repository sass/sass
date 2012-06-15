require 'set'

module Sass
  module SCSS
    # The parser for SCSS.
    # It parses a string of code into a tree of {Sass::Tree::Node}s.
    class Parser
      # @param str [String, StringScanner] The source document to parse.
      #   Note that `Parser` *won't* raise a nice error message if this isn't properly parsed;
      #   for that, you should use the higher-level {Sass::Engine} or {Sass::CSS}.
      # @param filename [String] The name of the file being parsed. Used for warnings.
      # @param line [Fixnum] The line on which the source string appeared,
      #   if it's part of another document.
      def initialize(str, filename, line = 1)
        @template = str
        @filename = filename
        @line = line
        @strs = []
      end

      # Parses an SCSS document.
      #
      # @return [Sass::Tree::RootNode] The root node of the document tree
      # @raise [Sass::SyntaxError] if there's a syntax error in the document
      def parse
        init_scanner!
        root = stylesheet
        expected("selector or at-rule") unless @scanner.eos?
        root
      end

      # Parses an identifier with interpolation.
      # Note that this won't assert that the identifier takes up the entire input string;
      # it's meant to be used with `StringScanner`s as part of other parsers.
      #
      # @return [Array<String, Sass::Script::Node>, nil]
      #   The interpolated identifier, or nil if none could be parsed
      def parse_interp_ident
        init_scanner!
        interp_ident
      end

      private

      include Sass::SCSS::RX

      def init_scanner!
        @scanner =
          if @template.is_a?(StringScanner)
            @template
          else
            Sass::Util::MultibyteStringScanner.new(@template.gsub("\r", ""))
          end
      end

      def stylesheet
        node = node(Sass::Tree::RootNode.new(@scanner.string))
        block_contents(node, :stylesheet) {s(node)}
      end

      def s(node)
        while tok(S) || tok(CDC) || tok(CDO) || (c = tok(SINGLE_LINE_COMMENT)) || (c = tok(COMMENT))
          next unless c
          process_comment c, node
          c = nil
        end
        true
      end

      def ss
        nil while tok(S) || tok(SINGLE_LINE_COMMENT) || tok(COMMENT)
        true
      end

      def ss_comments(node)
        while tok(S) || (c = tok(SINGLE_LINE_COMMENT)) || (c = tok(COMMENT))
          next unless c
          process_comment c, node
          c = nil
        end

        true
      end

      def whitespace
        return unless tok(S) || tok(SINGLE_LINE_COMMENT) || tok(COMMENT)
        ss
      end

      def process_comment(text, node)
        silent = text =~ /^\/\//
        line = @line - text.count("\n")
        if loud = text =~ %r{^/[/*]!}
          value = Sass::Engine.parse_interp(text, line, @scanner.pos - text.size, :filename => @filename)
          value[0].slice!(2) # get rid of the "!"
        else
          value = [text]
        end

        if silent
          value = Sass::Util.with_extracted_values(value) do |str|
            str.sub(/^\s*\/\//, '/*').gsub(/^\s*\/\//, ' *') + ' */'
          end
        else
          value.unshift(@scanner.
            string[0...@scanner.pos].
            reverse[/.*?\*\/(.*?)($|\Z)/, 1].
            reverse.gsub(/[^\s]/, ' '))
        end

        comment = Sass::Tree::CommentNode.new(value, silent, loud)
        comment.line = line
        node << comment
      end

      DIRECTIVES = Set[:mixin, :include, :function, :return, :debug, :warn, :for,
        :each, :while, :if, :else, :extend, :import, :media, :charset, :_moz_document]

      PREFIXED_DIRECTIVES = Set[:supports]

      def directive
        return unless tok(/@/)
        name = tok!(IDENT)
        ss

        if dir = special_directive(name)
          return dir
        elsif dir = prefixed_directive(name)
          return dir
        end

        # Most at-rules take expressions (e.g. @import),
        # but some (e.g. @page) take selector-like arguments
        val = str {break unless expr}
        val ||= CssParser.new(@scanner, @line).parse_selector_string
        directive_body("@#{name} #{val}")
      end

      def directive_body(value)
        node = node(Sass::Tree::DirectiveNode.new(value.strip))

        if tok(/\{/)
          node.has_children = true
          block_contents(node, :directive)
          tok!(/\}/)
        end

        node
      end

      def special_directive(name)
        sym = name.gsub('-', '_').to_sym
        DIRECTIVES.include?(sym) && send("#{sym}_directive")
      end

      def prefixed_directive(name)
        sym = name.gsub(/^-[a-z0-9]+-/i, '').gsub('-', '_').to_sym
        PREFIXED_DIRECTIVES.include?(sym) && send("#{sym}_directive", name)
      end

      def mixin_directive
        name = tok! IDENT
        args = sass_script(:parse_mixin_definition_arglist)
        ss
        block(node(Sass::Tree::MixinDefNode.new(name, args)), :directive)
      end

      def include_directive
        name = tok! IDENT
        args, keywords = sass_script(:parse_mixin_include_arglist)
        ss
        node(Sass::Tree::MixinNode.new(name, args, keywords))
      end

      def function_directive
        name = tok! IDENT
        args = sass_script(:parse_function_definition_arglist)
        ss
        block(node(Sass::Tree::FunctionNode.new(name, args)), :function)
      end

      def return_directive
        node(Sass::Tree::ReturnNode.new(sass_script(:parse)))
      end

      def debug_directive
        node(Sass::Tree::DebugNode.new(sass_script(:parse)))
      end

      def warn_directive
        node(Sass::Tree::WarnNode.new(sass_script(:parse)))
      end

      def for_directive
        tok!(/\$/)
        var = tok! IDENT
        ss

        tok!(/from/)
        from = sass_script(:parse_until, Set["to", "through"])
        ss

        @expected = '"to" or "through"'
        exclusive = (tok(/to/) || tok!(/through/)) == 'to'
        to = sass_script(:parse)
        ss

        block(node(Sass::Tree::ForNode.new(var, from, to, exclusive)), :directive)
      end

      def each_directive
        tok!(/\$/)
        var = tok! IDENT
        ss

        tok!(/in/)
        list = sass_script(:parse)
        ss

        block(node(Sass::Tree::EachNode.new(var, list)), :directive)
      end

      def while_directive
        expr = sass_script(:parse)
        ss
        block(node(Sass::Tree::WhileNode.new(expr)), :directive)
      end

      def if_directive
        expr = sass_script(:parse)
        ss
        node = block(node(Sass::Tree::IfNode.new(expr)), :directive)
        pos = @scanner.pos
        line = @line
        ss

        else_block(node) ||
          begin
            # Backtrack in case there are any comments we want to parse
            @scanner.pos = pos
            @line = line
            node
          end
      end

      def else_block(node)
        return unless tok(/@else/)
        ss
        else_node = block(
          Sass::Tree::IfNode.new((sass_script(:parse) if tok(/if/))),
          :directive)
        node.add_else(else_node)
        pos = @scanner.pos
        line = @line
        ss

        else_block(node) ||
          begin
            # Backtrack in case there are any comments we want to parse
            @scanner.pos = pos
            @line = line
            node
          end
      end

      def else_directive
        err("Invalid CSS: @else must come after @if")
      end

      def extend_directive
        node(Sass::Tree::ExtendNode.new(expr!(:selector_sequence)))
      end

      def import_directive
        values = []

        loop do
          values << expr!(:import_arg)
          break if use_css_import? || !tok(/,\s*/)
        end

        return values
      end

      def import_arg
        return unless arg = tok(STRING) || (uri = tok!(URI))
        path = @scanner[1] || @scanner[2] || @scanner[3]
        ss

        media = str {media_query_list}.strip

        if uri || path =~ /^http:\/\// || !media.strip.empty? || use_css_import?
          return node(Sass::Tree::DirectiveNode.new("@import #{arg} #{media}".strip))
        end

        node(Sass::Tree::ImportNode.new(path.strip))
      end

      def use_css_import?; false; end

      def media_directive
        block(node(Sass::Tree::MediaNode.new(media_query_list)), :directive)
      end

      # http://www.w3.org/TR/css3-mediaqueries/#syntax
      def media_query_list
        has_q = false
        q = str {has_q = media_query}

        return unless has_q
        queries = [q.strip]

        ss
        while tok(/,/)
          ss; queries << str {expr!(:media_query)}.strip; ss
        end

        queries
      end

      def media_query
        if tok(/only|not/i)
          ss
          @expected = "media type (e.g. print, screen)"
          tok!(IDENT)
          ss
        elsif !tok(IDENT) && !media_expr
          return
        end

        ss
        while tok(/and/i)
          ss; expr!(:media_expr); ss
        end

        true
      end

      def media_expr
        return unless tok(/\(/)
        ss
        @expected = "media feature (e.g. min-device-width, color)"
        tok!(IDENT)
        ss

        if tok(/:/)
          ss; expr!(:expr)
        end
        tok!(/\)/)
        ss

        true
      end

      def charset_directive
        tok! STRING
        name = @scanner[1] || @scanner[2]
        ss
        node(Sass::Tree::CharsetNode.new(name))
      end

      # The document directive is specified in
      # http://www.w3.org/TR/css3-conditional/, but Gecko allows the
      # `url-prefix` and `domain` functions to omit quotation marks, contrary to
      # the standard.
      #
      # We could parse all document directives according to Mozilla's syntax,
      # but if someone's using e.g. @-webkit-document we don't want them to
      # think WebKit works sans quotes.
      def _moz_document_directive
        value = str do
          begin
            ss
            expr!(:moz_document_function)
          end while tok(/,/)
        end
        directive_body("@-moz-document #{value}")
      end

      def moz_document_function
        return unless tok(URI) || tok(URL_PREFIX) || tok(DOMAIN) || function
        ss
      end

      # http://www.w3.org/TR/css3-conditional/
      def supports_directive(name)
        value = str {expr!(:supports_condition)}
        directive_body("@#{name} #{value}")
      end

      def supports_condition
        supports_negation || supports_operator || supports_declaration_condition
      end

      def supports_negation
        return unless tok(/not/i)
        ss
        expr!(:supports_condition_in_parens)
      end

      def supports_operator
        return unless supports_condition_in_parens
        tok!(/and|or/i)
        begin
          ss
          expr!(:supports_condition_in_parens)
        end while tok(/and|or/i)
        true
      end

      def supports_condition_in_parens
        return unless tok(/\(/); ss
        if supports_condition
          tok!(/\)/); ss
        else
          supports_declaration_body
        end
      end

      def supports_declaration_condition
        return unless tok(/\(/); ss
        supports_declaration_body
      end

      def supports_declaration_body
        tok!(IDENT); ss
        tok!(/:/); ss
        expr!(:expr); ss
        tok!(/\)/); ss
      end

      def variable
        return unless tok(/\$/)
        name = tok!(IDENT)
        ss; tok!(/:/); ss

        expr = sass_script(:parse)
        guarded = tok(DEFAULT)
        node(Sass::Tree::VariableNode.new(name, expr, guarded))
      end

      def operator
        # Many of these operators (all except / and ,)
        # are disallowed by the CSS spec,
        # but they're included here for compatibility
        # with some proprietary MS properties
        str {ss if tok(/[\/,:.=]/)}
      end

      def unary_operator
        tok(/[+-]/)
      end

      def ruleset
        return unless rules = selector_sequence
        block(node(Sass::Tree::RuleNode.new(rules.flatten.compact)), :ruleset)
      end

      def block(node, context)
        node.has_children = true
        tok!(/\{/)
        block_contents(node, context)
        tok!(/\}/)
        node
      end

      # A block may contain declarations and/or rulesets
      def block_contents(node, context)
        block_given? ? yield : ss_comments(node)
        node << (child = block_child(context))
        while tok(/;/) || has_children?(child)
          block_given? ? yield : ss_comments(node)
          node << (child = block_child(context))
        end
        node
      end

      def block_child(context)
        return variable || directive if context == :function
        return variable || directive || ruleset if context == :stylesheet
        variable || directive || declaration_or_ruleset
      end

      def has_children?(child_or_array)
        return false unless child_or_array
        return child_or_array.last.has_children if child_or_array.is_a?(Array)
        return child_or_array.has_children
      end

      # This is a nasty hack, and the only place in the parser
      # that requires backtracking.
      # The reason is that we can't figure out if certain strings
      # are declarations or rulesets with fixed finite lookahead.
      # For example, "foo:bar baz baz baz..." could be either a property
      # or a selector.
      #
      # To handle this, we simply check if it works as a property
      # (which is the most common case)
      # and, if it doesn't, try it as a ruleset.
      #
      # We could eke some more efficiency out of this
      # by handling some easy cases (first token isn't an identifier,
      # no colon after the identifier, whitespace after the colon),
      # but I'm not sure the gains would be worth the added complexity.
      def declaration_or_ruleset
        old_use_property_exception, @use_property_exception =
          @use_property_exception, false
        decl_err = catch_error do
          decl = declaration
          unless decl && decl.has_children
            # We want an exception if it's not there,
            # but we don't want to consume if it is
            tok!(/[;}]/) unless tok?(/[;}]/)
          end
          return decl
        end

        ruleset_err = catch_error {return ruleset}
        rethrow(@use_property_exception ? decl_err : ruleset_err)
      ensure
        @use_property_exception = old_use_property_exception
      end

      def selector_sequence
        if sel = tok(STATIC_SELECTOR, true)
          return [sel]
        end

        rules = []
        return unless v = selector
        rules.concat v

        ws = ''
        while tok(/,/)
          ws << str {ss}
          if v = selector
            rules << ',' << ws
            rules.concat v
            ws = ''
          end
        end
        rules
      end

      def selector
        return unless sel = _selector
        sel.to_a
      end

      def selector_comma_sequence
        return unless sel = _selector
        selectors = [sel]
        ws = ''
        while tok(/,/)
          ws << str{ss}
          if sel = _selector
            selectors << sel
            selectors[-1] = Selector::Sequence.new(["\n"] + selectors.last.members) if ws.include?("\n")
            ws = ''
          end
        end
        Selector::CommaSequence.new(selectors)
      end

      def _selector
        # The combinator here allows the "> E" hack
        return unless val = combinator || simple_selector_sequence
        nl = str{ss}.include?("\n")
        res = []
        res << val
        res << "\n" if nl

        while val = combinator || simple_selector_sequence
          res << val
          res << "\n" if str{ss}.include?("\n")
        end
        Selector::Sequence.new(res.compact)
      end

      def combinator
        tok(PLUS) || tok(GREATER) || tok(TILDE)
      end

      def simple_selector_sequence
        # This allows for stuff like http://www.w3.org/TR/css3-animations/#keyframes-
        return expr unless e = element_name || id_selector || class_selector ||
          attrib || negation || pseudo || parent_selector || interpolation_selector
        res = [e]

        # The tok(/\*/) allows the "E*" hack
        while v = id_selector || class_selector || attrib || negation || pseudo ||
            interpolation_selector || (tok(/\*/) && Selector::Universal.new(nil))
          res << v
        end

        pos = @scanner.pos
        line = @line
        if sel = str? {simple_selector_sequence}
          @scanner.pos = pos
          @line = line

          if sel =~ /^&/
            begin
              throw_error {expected('"{"')}
            rescue Sass::SyntaxError => e
              e.message << "\n\n\"#{sel}\" may only be used at the beginning of a selector."
              raise e
            end
          else
            Sass::Util.sass_warn(<<MESSAGE)
DEPRECATION WARNING:
On line #{@line}#{" of \"#{@filename}\"" if @filename}, after "#{self.class.prior_snippet(@scanner)}"
Starting in Sass 3.2, "#{sel}" may only be used at the beginning of a selector.
MESSAGE
          end
        end

        Selector::SimpleSequence.new(res)
      end

      def parent_selector
        return unless tok(/&/)
        Selector::Parent.new
      end

      def class_selector
        return unless tok(/\./)
        @expected = "class name"
        Selector::Class.new(merge(expr!(:interp_ident)))
      end

      def id_selector
        return unless tok(/#(?!\{)/)
        @expected = "id name"
        Selector::Id.new(merge(expr!(:interp_name)))
      end

      def element_name
        return unless name = interp_ident || tok(/\*/) || (tok?(/\|/) && "")
        if tok(/\|/)
          @expected = "element name or *"
          ns = name
          name = interp_ident || tok!(/\*/)
        end

        if name == '*'
          Selector::Universal.new(merge(ns))
        else
          Selector::Element.new(merge(name), merge(ns))
        end
      end

      def interpolation_selector
        return unless script = interpolation
        Selector::Interpolation.new(script)
      end

      def attrib
        return unless tok(/\[/)
        ss
        ns, name = attrib_name!
        ss

        if op = tok(/=/) ||
            tok(INCLUDES) ||
            tok(DASHMATCH) ||
            tok(PREFIXMATCH) ||
            tok(SUFFIXMATCH) ||
            tok(SUBSTRINGMATCH)
          @expected = "identifier or string"
          ss
          val = interp_ident || expr!(:interp_string)
          ss
        end
        tok!(/\]/)

        Selector::Attribute.new(merge(name), merge(ns), op, merge(val))
      end

      def attrib_name!
        if name_or_ns = interp_ident
          # E, E|E
          if tok(/\|(?!=)/)
            ns = name_or_ns
            name = interp_ident
          else
            name = name_or_ns
          end
        else
          # *|E or |E
          ns = [tok(/\*/) || ""]
          tok!(/\|/)
          name = expr!(:interp_ident)
        end
        return ns, name
      end

      def pseudo
        return unless s = tok(/::?/)
        @expected = "pseudoclass or pseudoelement"
        name = expr!(:interp_ident)
        if tok(/\(/)
          ss
          arg = expr!(:pseudo_expr)
          tok!(/\)/)
        end
        Selector::Pseudo.new(s == ':' ? :class : :element, merge(name), merge(arg))
      end

      def pseudo_expr
        return unless e = tok(PLUS) || tok(/-/) || tok(NUMBER) ||
          interp_string || tok(IDENT) || interpolation
        res = [e, str{ss}]
        while e = tok(PLUS) || tok(/-/) || tok(NUMBER) ||
            interp_string || tok(IDENT) || interpolation
          res << e << str{ss}
        end
        res
      end

      def negation
        return unless name = tok(NOT) || tok(MOZ_ANY)
        ss
        @expected = "selector"
        sel = selector_comma_sequence
        tok!(/\)/)
        Selector::SelectorPseudoClass.new(name[1...-1], sel)
      end

      def declaration
        # This allows the "*prop: val", ":prop: val", and ".prop: val" hacks
        if s = tok(/[:\*\.]|\#(?!\{)/)
          @use_property_exception = s !~ /[\.\#]/
          name = [s, str{ss}, *expr!(:interp_ident)]
        else
          return unless name = interp_ident
          name = [name] if name.is_a?(String)
        end
        if comment = tok(COMMENT)
          name << comment
        end
        ss

        tok!(/:/)
        space, value = value!
        ss
        require_block = tok?(/\{/)

        node = node(Sass::Tree::PropNode.new(name.flatten.compact, value, :new))

        return node unless require_block
        nested_properties! node, space
      end

      def value!
        space = !str {ss}.empty?
        @use_property_exception ||= space || !tok?(IDENT)

        return true, Sass::Script::String.new("") if tok?(/\{/)
        # This is a bit of a dirty trick:
        # if the value is completely static,
        # we don't parse it at all, and instead return a plain old string
        # containing the value.
        # This results in a dramatic speed increase.
        if val = tok(STATIC_VALUE, true)
          return space, Sass::Script::String.new(val.strip)
        end
        return space, sass_script(:parse)
      end

      def plain_value
        return unless tok(/:/)
        space = !str {ss}.empty?
        @use_property_exception ||= space || !tok?(IDENT)

        expression = expr
        expression << tok(IMPORTANT) if expression
        # expression, space, value
        return expression, space, expression || [""]
      end

      def nested_properties!(node, space)
        err(<<MESSAGE) unless space
Invalid CSS: a space is required between a property and its definition
when it has other properties nested beneath it.
MESSAGE

        @use_property_exception = true
        @expected = 'expression (e.g. 1px, bold) or "{"'
        block(node, :property)
      end

      def expr
        return unless t = term
        res = [t, str{ss}]

        while (o = operator) && (t = term)
          res << o << t << str{ss}
        end

        res
      end

      def term
        unless e = tok(NUMBER) ||
            tok(URI) ||
            function ||
            tok(STRING) ||
            tok(UNICODERANGE) ||
            tok(IDENT) ||
            tok(HEXCOLOR)

          return unless op = unary_operator
          @expected = "number or function"
          return [op, tok(NUMBER) || expr!(:function)]
        end
        e
      end

      def function
        return unless name = tok(FUNCTION)
        if name == "expression(" || name == "calc("
          str, _ = Sass::Shared.balance(@scanner, ?(, ?), 1)
          [name, str]
        else
          [name, str{ss}, expr, tok!(/\)/)]
        end
      end

      def interpolation
        return unless tok(INTERP_START)
        sass_script(:parse_interpolated)
      end

      def interp_string
        _interp_string(:double) || _interp_string(:single)
      end

      def _interp_string(type)
        return unless start = tok(Sass::Script::Lexer::STRING_REGULAR_EXPRESSIONS[[type, false]])
        res = [start]

        mid_re = Sass::Script::Lexer::STRING_REGULAR_EXPRESSIONS[[type, true]]
        # @scanner[2].empty? means we've started an interpolated section
        while @scanner[2] == '#{'
          @scanner.pos -= 2 # Don't consume the #{
          res.last.slice!(-2..-1)
          res << expr!(:interpolation) << tok(mid_re)
        end
        res
      end

      def interp_ident(start = IDENT)
        return unless val = tok(start) || interpolation || tok(IDENT_HYPHEN_INTERP, true)
        res = [val]
        while val = tok(NAME) || interpolation
          res << val
        end
        res
      end

      def interp_name
        interp_ident NAME
      end

      def str
        @strs.push ""
        yield
        @strs.last
      ensure
        @strs.pop
      end

      def str?
        pos = @scanner.pos
        line = @line
        @strs.push ""
        throw_error {yield} && @strs.last
      rescue Sass::SyntaxError => e
        @scanner.pos = pos
        @line = line
        nil
      ensure
        @strs.pop
      end

      def node(node)
        node.line = @line
        node
      end

      @sass_script_parser = Class.new(Sass::Script::Parser)
      @sass_script_parser.send(:include, ScriptParser)
      # @private
      def self.sass_script_parser; @sass_script_parser; end

      def sass_script(*args)
        parser = self.class.sass_script_parser.new(@scanner, @line,
          @scanner.pos - (@scanner.string[0...@scanner.pos].rindex("\n") || 0))
        result = parser.send(*args)
        @line = parser.line
        result
      rescue Sass::SyntaxError => e
        throw(:_sass_parser_error, true) if @throw_error
        raise e
      end

      def merge(arr)
        arr && Sass::Util.merge_adjacent_strings([arr].flatten)
      end

      EXPR_NAMES = {
        :media_query => "media query (e.g. print, screen, print and screen)",
        :media_expr => "media expression (e.g. (min-device-width: 800px)))",
        :pseudo_expr => "expression (e.g. fr, 2n+1)",
        :interp_ident => "identifier",
        :interp_name => "identifier",
        :expr => "expression (e.g. 1px, bold)",
        :_selector => "selector",
        :selector_comma_sequence => "selector",
        :simple_selector_sequence => "selector",
        :import_arg => "file to import (string or url())",
        :moz_document_function => "matching function (e.g. url-prefix(), domain())",
        :supports_condition => "@supports condition (e.g. (display: flexbox))",
        :supports_condition_in_parens => "@supports condition (e.g. (display: flexbox))",
      }

      TOK_NAMES = Sass::Util.to_hash(
        Sass::SCSS::RX.constants.map {|c| [Sass::SCSS::RX.const_get(c), c.downcase]}).
        merge(IDENT => "identifier", /[;}]/ => '";"')

      def tok?(rx)
        @scanner.match?(rx)
      end

      def expr!(name)
        (e = send(name)) && (return e)
        expected(EXPR_NAMES[name] || name.to_s)
      end

      def tok!(rx)
        (t = tok(rx)) && (return t)
        name = TOK_NAMES[rx]

        unless name
          # Display basic regexps as plain old strings
          string = rx.source.gsub(/\\(.)/, '\1')
          name = rx.source == Regexp.escape(string) ? string.inspect : rx.inspect
        end

        expected(name)
      end

      def expected(name)
        throw(:_sass_parser_error, true) if @throw_error
        self.class.expected(@scanner, @expected || name, @line)
      end

      def err(msg)
        throw(:_sass_parser_error, true) if @throw_error
        raise Sass::SyntaxError.new(msg, :line => @line)
      end

      def throw_error
        old_throw_error, @throw_error = @throw_error, false
        yield
      ensure
        @throw_error = old_throw_error
      end

      def catch_error(&block)
        old_throw_error, @throw_error = @throw_error, true
        pos = @scanner.pos
        line = @line
        expected = @expected
        if catch(:_sass_parser_error, &block)
          @scanner.pos = pos
          @line = line
          @expected = expected
          {:pos => pos, :line => line, :expected => @expected, :block => block}
        end
      ensure
        @throw_error = old_throw_error
      end

      def rethrow(err)
        if @throw_err
          throw :_sass_parser_error, err
        else
          @scanner = Sass::Util::MultibyteStringScanner.new(@scanner.string)
          @scanner.pos = err[:pos]
          @line = err[:line]
          @expected = err[:expected]
          err[:block].call
        end
      end

      # @private
      def self.expected(scanner, expected, line)
        was = scanner.rest.dup
        # Get rid of whitespace between pos and the next token,
        # but only if there's a newline in there
        was.gsub!(/^\s*\n\s*/, '')
        # Also get rid of stuff after the next newline
        was.gsub!(/\n.*/, '')
        was = was[0...15] + "..." if was.size > 18

        raise Sass::SyntaxError.new(
          "Invalid CSS after \"#{prior_snippet(scanner)}\": expected #{expected}, was \"#{was}\"",
          :line => line)
      end

      # @private
      def self.prior_snippet(scanner)
        pos = scanner.pos

        after = scanner.string[0...pos]
        # Get rid of whitespace between pos and the last token,
        # but only if there's a newline in there
        after.gsub!(/\s*\n\s*$/, '')
        # Also get rid of stuff before the last newline
        after.gsub!(/.*\n/, '')
        after = "..." + after[-15..-1] if after.size > 18
        after
      end

      # Avoid allocating lots of new strings for `#tok`.
      # This is important because `#tok` is called all the time.
      NEWLINE = "\n"

      def tok(rx, last_group_lookahead = false)
        res = @scanner.scan(rx)
        if res
          # This fixes https://github.com/nex3/sass/issues/104, which affects
          # Ruby 1.8.7 and REE. This fix is to replace the ?= zero-width
          # positive lookahead operator in the Regexp (which matches without
          # consuming the matched group), with a match that does consume the
          # group, but then rewinds the scanner and removes the group from the
          # end of the matched string. This fix makes the assumption that the
          # matched group will always occur at the end of the match.
          if last_group_lookahead && @scanner[-1]
            @scanner.pos -= @scanner[-1].length
            res.slice!(-@scanner[-1].length..-1)
          end
          @line += res.count(NEWLINE)
          @expected = nil
          if !@strs.empty? && rx != COMMENT && rx != SINGLE_LINE_COMMENT
            @strs.each {|s| s << res}
          end
          res
        end
      end
    end
  end
end
