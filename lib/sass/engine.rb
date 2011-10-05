require 'strscan'
require 'set'
require 'digest/sha1'
require 'sass/cache_stores'
require 'sass/tree/node'
require 'sass/tree/root_node'
require 'sass/tree/rule_node'
require 'sass/tree/comment_node'
require 'sass/tree/prop_node'
require 'sass/tree/directive_node'
require 'sass/tree/media_node'
require 'sass/tree/variable_node'
require 'sass/tree/mixin_def_node'
require 'sass/tree/mixin_node'
require 'sass/tree/function_node'
require 'sass/tree/return_node'
require 'sass/tree/extend_node'
require 'sass/tree/if_node'
require 'sass/tree/while_node'
require 'sass/tree/for_node'
require 'sass/tree/each_node'
require 'sass/tree/debug_node'
require 'sass/tree/warn_node'
require 'sass/tree/import_node'
require 'sass/tree/charset_node'
require 'sass/tree/visitors/base'
require 'sass/tree/visitors/perform'
require 'sass/tree/visitors/cssize'
require 'sass/tree/visitors/convert'
require 'sass/tree/visitors/to_css'
require 'sass/tree/visitors/deep_copy'
require 'sass/tree/visitors/set_options'
require 'sass/tree/visitors/check_nesting'
require 'sass/selector'
require 'sass/environment'
require 'sass/script'
require 'sass/scss'
require 'sass/error'
require 'sass/importers'
require 'sass/shared'

module Sass

  # A Sass mixin or function.
  #
  # `name`: `String`
  # : The name of the mixin/function.
  #
  # `args`: `Array<(String, Script::Node)>`
  # : The arguments for the mixin/function.
  #   Each element is a tuple containing the name of the argument
  #   and the parse tree for the default value of the argument.
  #
  # `environment`: {Sass::Environment}
  # : The environment in which the mixin/function was defined.
  #   This is captured so that the mixin/function can have access
  #   to local variables defined in its scope.
  #
  # `tree`: `Array<Tree::Node>`
  # : The parse tree for the mixin/function.
  Callable = Struct.new(:name, :args, :environment, :tree)

  # This class handles the parsing and compilation of the Sass template.
  # Example usage:
  #
  #     template = File.load('stylesheets/sassy.sass')
  #     sass_engine = Sass::Engine.new(template)
  #     output = sass_engine.render
  #     puts output
  class Engine
    include Sass::Util

    # A line of Sass code.
    #
    # `text`: `String`
    # : The text in the line, without any whitespace at the beginning or end.
    #
    # `tabs`: `Fixnum`
    # : The level of indentation of the line.
    #
    # `index`: `Fixnum`
    # : The line number in the original document.
    #
    # `offset`: `Fixnum`
    # : The number of bytes in on the line that the text begins.
    #   This ends up being the number of bytes of leading whitespace.
    #
    # `filename`: `String`
    # : The name of the file in which this line appeared.
    #
    # `children`: `Array<Line>`
    # : The lines nested below this one.
    #
    # `comment_tab_str`: `String?`
    # : The prefix indentation for this comment, if it is a comment.
    class Line < Struct.new(:text, :tabs, :index, :offset, :filename, :children, :comment_tab_str)
      def comment?
        text[0] == COMMENT_CHAR && (text[1] == SASS_COMMENT_CHAR || text[1] == CSS_COMMENT_CHAR)
      end
    end

    # The character that begins a CSS property.
    PROPERTY_CHAR  = ?:

    # The character that designates the beginning of a comment,
    # either Sass or CSS.
    COMMENT_CHAR = ?/

    # The character that follows the general COMMENT_CHAR and designates a Sass comment,
    # which is not output as a CSS comment.
    SASS_COMMENT_CHAR = ?/

    # The character that indicates that a comment allows interpolation
    # and should be preserved even in `:compressed` mode.
    SASS_LOUD_COMMENT_CHAR = ?!

    # The character that follows the general COMMENT_CHAR and designates a CSS comment,
    # which is embedded in the CSS document.
    CSS_COMMENT_CHAR = ?*

    # The character used to denote a compiler directive.
    DIRECTIVE_CHAR = ?@

    # Designates a non-parsed rule.
    ESCAPE_CHAR    = ?\\

    # Designates block as mixin definition rather than CSS rules to output
    MIXIN_DEFINITION_CHAR = ?=

    # Includes named mixin declared using MIXIN_DEFINITION_CHAR
    MIXIN_INCLUDE_CHAR    = ?+

    # The regex that matches and extracts data from
    # properties of the form `:name prop`.
    PROPERTY_OLD = /^:([^\s=:"]+)\s*(?:\s+|$)(.*)/

    # The default options for Sass::Engine.
    # @api public
    DEFAULT_OPTIONS = {
      :style => :nested,
      :load_paths => ['.'],
      :cache => true,
      :cache_location => './.sass-cache',
      :syntax => :sass,
      :filesystem_importer => Sass::Importers::Filesystem
    }.freeze

    # Converts a Sass options hash into a standard form, filling in
    # default values and resolving aliases.
    #
    # @param options [{Symbol => Object}] The options hash;
    #   see {file:SASS_REFERENCE.md#sass_options the Sass options documentation}
    # @return [{Symbol => Object}] The normalized options hash.
    # @private
    def self.normalize_options(options)
      options = DEFAULT_OPTIONS.merge(options.reject {|k, v| v.nil?})

      # If the `:filename` option is passed in without an importer,
      # assume it's using the default filesystem importer.
      options[:importer] ||= options[:filesystem_importer].new(".") if options[:filename]

      # Tracks the original filename of the top-level Sass file
      options[:original_filename] ||= options[:filename]

      options[:cache_store] ||= Sass::CacheStores::Chain.new(
        Sass::CacheStores::Memory.new, Sass::CacheStores::Filesystem.new(options[:cache_location]))
      # Support both, because the docs said one and the other actually worked
      # for quite a long time.
      options[:line_comments] ||= options[:line_numbers]

      options[:load_paths] = options[:load_paths].map do |p|
        next p unless p.is_a?(String) || (defined?(Pathname) && p.is_a?(Pathname))
        options[:filesystem_importer].new(p.to_s)
      end

      # Backwards compatibility
      options[:property_syntax] ||= options[:attribute_syntax]
      case options[:property_syntax]
      when :alternate; options[:property_syntax] = :new
      when :normal; options[:property_syntax] = :old
      end

      options
    end

    # Returns the {Sass::Engine} for the given file.
    # This is preferable to Sass::Engine.new when reading from a file
    # because it properly sets up the Engine's metadata,
    # enables parse-tree caching,
    # and infers the syntax from the filename.
    #
    # @param filename [String] The path to the Sass or SCSS file
    # @param options [{Symbol => Object}] The options hash;
    #   See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    # @return [Sass::Engine] The Engine for the given Sass or SCSS file.
    # @raise [Sass::SyntaxError] if there's an error in the document.
    def self.for_file(filename, options)
      had_syntax = options[:syntax]

      if had_syntax
        # Use what was explicitly specificed
      elsif filename =~ /\.scss$/
        options.merge!(:syntax => :scss)
      elsif filename =~ /\.sass$/
        options.merge!(:syntax => :sass)
      end

      Sass::Engine.new(File.read(filename), options.merge(:filename => filename))
    end

    # The options for the Sass engine.
    # See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    #
    # @return [{Symbol => Object}]
    attr_reader :options

    # Creates a new Engine. Note that Engine should only be used directly
    # when compiling in-memory Sass code.
    # If you're compiling a single Sass file from the filesystem,
    # use \{Sass::Engine.for\_file}.
    # If you're compiling multiple files from the filesystem,
    # use {Sass::Plugin.
    #
    # @param template [String] The Sass template.
    #   This template can be encoded using any encoding
    #   that can be converted to Unicode.
    #   If the template contains an `@charset` declaration,
    #   that overrides the Ruby encoding
    #   (see {file:SASS_REFERENCE.md#encodings the encoding documentation})
    # @param options [{Symbol => Object}] An options hash.
    #   See {file:SASS_REFERENCE.md#sass_options the Sass options documentation}.
    # @see {Sass::Engine.for_file}
    # @see {Sass::Plugin}
    def initialize(template, options={})
      @options = self.class.normalize_options(options)
      @template = template
    end

    # Render the template to CSS.
    #
    # @return [String] The CSS
    # @raise [Sass::SyntaxError] if there's an error in the document
    # @raise [Encoding::UndefinedConversionError] if the source encoding
    #   cannot be converted to UTF-8
    # @raise [ArgumentError] if the document uses an unknown encoding with `@charset`
    def render
      return _render unless @options[:quiet]
      Sass::Util.silence_sass_warnings {_render}
    end
    alias_method :to_css, :render

    # Parses the document into its parse tree. Memoized.
    #
    # @return [Sass::Tree::Node] The root of the parse tree.
    # @raise [Sass::SyntaxError] if there's an error in the document
    def to_tree
      @tree ||= @options[:quiet] ?
        Sass::Util.silence_sass_warnings {_to_tree} :
        _to_tree
    end

    # Returns the original encoding of the document,
    # or `nil` under Ruby 1.8.
    #
    # @return [Encoding, nil]
    # @raise [Encoding::UndefinedConversionError] if the source encoding
    #   cannot be converted to UTF-8
    # @raise [ArgumentError] if the document uses an unknown encoding with `@charset`
    def source_encoding
      check_encoding!
      @original_encoding
    end

    # Gets a set of all the documents
    # that are (transitive) dependencies of this document,
    # not including the document itself.
    #
    # @return [[Sass::Engine]] The dependency documents.
    def dependencies
      _dependencies(Set.new, engines = Set.new)
      engines - [self]
    end

    # Helper for \{#dependencies}.
    #
    # @private
    def _dependencies(seen, engines)
      return if seen.include?(key = [@options[:filename], @options[:importer]])
      seen << key
      engines << self
      to_tree.grep(Tree::ImportNode) do |n|
        next if n.css_import?
        n.imported_file._dependencies(seen, engines)
      end
    end

    private

    def _render
      rendered = _to_tree.render
      return rendered if ruby1_8?
      begin
        # Try to convert the result to the original encoding,
        # but if that doesn't work fall back on UTF-8
        rendered = rendered.encode(source_encoding)
      rescue EncodingError
      end
      rendered.gsub(Regexp.new('\A@charset "(.*?)"'.encode(source_encoding)),
        "@charset \"#{source_encoding.name}\"".encode(source_encoding))
    end

    def _to_tree
      if (@options[:cache] || @options[:read_cache]) &&
          @options[:filename] && @options[:importer]
        key = sassc_key
        sha = Digest::SHA1.hexdigest(@template)

        if root = @options[:cache_store].retrieve(key, sha)
          root.options = @options
          return root
        end
      end

      check_encoding!

      if @options[:syntax] == :scss
        root = Sass::SCSS::Parser.new(@template, @options[:filename]).parse
      else
        root = Tree::RootNode.new(@template)
        append_children(root, tree(tabulate(@template)).first, true)
      end

      root.options = @options
      if @options[:cache] && key && sha
        begin
          old_options = root.options
          root.options = {}
          @options[:cache_store].store(key, sha, root)
        ensure
          root.options = old_options
        end
      end
      root
    rescue SyntaxError => e
      e.modify_backtrace(:filename => @options[:filename], :line => @line)
      e.sass_template = @template
      raise e
    end

    def sassc_key
      @options[:cache_store].key(*@options[:importer].key(@options[:filename], @options))
    end

    def check_encoding!
      return if @checked_encoding
      @checked_encoding = true
      @template, @original_encoding = check_sass_encoding(@template) do |msg, line|
        raise Sass::SyntaxError.new(msg, :line => line)
      end
    end

    def tabulate(string)
      tab_str = nil
      comment_tab_str = nil
      first = true
      lines = []
      string.gsub(/\r|\n|\r\n|\r\n/, "\n").scan(/^[^\n]*?$/).each_with_index do |line, index|
        index += (@options[:line] || 1)
        if line.strip.empty?
          lines.last.text << "\n" if lines.last && lines.last.comment?
          next
        end

        line_tab_str = line[/^\s*/]
        unless line_tab_str.empty?
          if tab_str.nil?
            comment_tab_str ||= line_tab_str
            next if try_comment(line, lines.last, "", comment_tab_str, index)
            comment_tab_str = nil
          end

          tab_str ||= line_tab_str

          raise SyntaxError.new("Indenting at the beginning of the document is illegal.",
            :line => index) if first

          raise SyntaxError.new("Indentation can't use both tabs and spaces.",
            :line => index) if tab_str.include?(?\s) && tab_str.include?(?\t)
        end
        first &&= !tab_str.nil?
        if tab_str.nil?
          lines << Line.new(line.strip, 0, index, 0, @options[:filename], [])
          next
        end

        comment_tab_str ||= line_tab_str
        if try_comment(line, lines.last, tab_str * lines.last.tabs, comment_tab_str, index)
          next
        else
          comment_tab_str = nil
        end

        line_tabs = line_tab_str.scan(tab_str).size
        if tab_str * line_tabs != line_tab_str
          message = <<END.strip.gsub("\n", ' ')
Inconsistent indentation: #{Sass::Shared.human_indentation line_tab_str, true} used for indentation,
but the rest of the document was indented using #{Sass::Shared.human_indentation tab_str}.
END
          raise SyntaxError.new(message, :line => index)
        end

        lines << Line.new(line.strip, line_tabs, index, tab_str.size, @options[:filename], [])
      end
      lines
    end

    def try_comment(line, last, tab_str, comment_tab_str, index)
      return unless last && last.comment?
      # Nested comment stuff must be at least one whitespace char deeper
      # than the normal indentation
      return unless line =~ /^#{tab_str}\s/
      unless line =~ /^(?:#{comment_tab_str})(.*)$/
        raise SyntaxError.new(<<MSG.strip.gsub("\n", " "), :line => index)
Inconsistent indentation:
previous line was indented by #{Sass::Shared.human_indentation comment_tab_str},
but this line was indented by #{Sass::Shared.human_indentation line[/^\s*/]}.
MSG
      end

      last.comment_tab_str ||= comment_tab_str
      last.text << "\n" << line
      true
    end

    def tree(arr, i = 0)
      return [], i if arr[i].nil?

      base = arr[i].tabs
      nodes = []
      while (line = arr[i]) && line.tabs >= base
        if line.tabs > base
          raise SyntaxError.new("The line was indented #{line.tabs - base} levels deeper than the previous line.",
            :line => line.index) if line.tabs > base + 1

          nodes.last.children, i = tree(arr, i)
        else
          nodes << line
          i += 1
        end
      end
      return nodes, i
    end

    def build_tree(parent, line, root = false)
      @line = line.index
      node_or_nodes = parse_line(parent, line, root)

      Array(node_or_nodes).each do |node|
        # Node is a symbol if it's non-outputting, like a variable assignment
        next unless node.is_a? Tree::Node

        node.line = line.index
        node.filename = line.filename

        append_children(node, line.children, false)
      end

      node_or_nodes
    end

    def append_children(parent, children, root)
      continued_rule = nil
      continued_comment = nil
      children.each do |line|
        child = build_tree(parent, line, root)

        if child.is_a?(Tree::RuleNode)
          if child.continued? && child.children.empty?
            if continued_rule
              continued_rule.add_rules child
            else
              continued_rule = child
            end
            next
          elsif continued_rule
            continued_rule.add_rules child
            continued_rule.children = child.children
            continued_rule, child = nil, continued_rule
          end
        elsif continued_rule
          continued_rule = nil
        end

        if child.is_a?(Tree::CommentNode) && child.silent
          if continued_comment &&
              child.line == continued_comment.line +
              continued_comment.lines + 1
            continued_comment.value += ["\n"] + child.value
            next
          end

          continued_comment = child
        end

        check_for_no_children(child)
        validate_and_append_child(parent, child, line, root)
      end

      parent
    end

    def validate_and_append_child(parent, child, line, root)
      case child
      when Array
        child.each {|c| validate_and_append_child(parent, c, line, root)}
      when Tree::Node
        parent << child
      end
    end

    def check_for_no_children(node)
      return unless node.is_a?(Tree::RuleNode) && node.children.empty?
      Sass::Util.sass_warn(<<WARNING.strip)
WARNING on line #{node.line}#{" of #{node.filename}" if node.filename}:
This selector doesn't have any properties and will not be rendered.
WARNING
    end

    def parse_line(parent, line, root)
      case line.text[0]
      when PROPERTY_CHAR
        if line.text[1] == PROPERTY_CHAR ||
            (@options[:property_syntax] == :new &&
             line.text =~ PROPERTY_OLD && $2.empty?)
          # Support CSS3-style pseudo-elements,
          # which begin with ::,
          # as well as pseudo-classes
          # if we're using the new property syntax
          Tree::RuleNode.new(parse_interp(line.text))
        else
          name, value = line.text.scan(PROPERTY_OLD)[0]
          raise SyntaxError.new("Invalid property: \"#{line.text}\".",
            :line => @line) if name.nil? || value.nil?
          parse_property(name, parse_interp(name), value, :old, line)
        end
      when ?$
        parse_variable(line)
      when COMMENT_CHAR
        parse_comment(line)
      when DIRECTIVE_CHAR
        parse_directive(parent, line, root)
      when ESCAPE_CHAR
        Tree::RuleNode.new(parse_interp(line.text[1..-1]))
      when MIXIN_DEFINITION_CHAR
        parse_mixin_definition(line)
      when MIXIN_INCLUDE_CHAR
        if line.text[1].nil? || line.text[1] == ?\s
          Tree::RuleNode.new(parse_interp(line.text))
        else
          parse_mixin_include(line, root)
        end
      else
        parse_property_or_rule(line)
      end
    end

    def parse_property_or_rule(line)
      scanner = StringScanner.new(line.text)
      hack_char = scanner.scan(/[:\*\.]|\#(?!\{)/)
      parser = Sass::SCSS::SassParser.new(scanner, @options[:filename], @line)

      unless res = parser.parse_interp_ident
        return Tree::RuleNode.new(parse_interp(line.text))
      end
      res.unshift(hack_char) if hack_char
      if comment = scanner.scan(Sass::SCSS::RX::COMMENT)
        res << comment
      end

      name = line.text[0...scanner.pos]
      if scanner.scan(/\s*:(?:\s|$)/)
        parse_property(name, res, scanner.rest, :new, line)
      else
        res.pop if comment
        Tree::RuleNode.new(res + parse_interp(scanner.rest))
      end
    end

    def parse_property(name, parsed_name, value, prop, line)
      if value.strip.empty?
        expr = Sass::Script::String.new("")
      else
        expr = parse_script(value, :offset => line.offset + line.text.index(value))
      end
      Tree::PropNode.new(parse_interp(name), expr, prop)
    end

    def parse_variable(line)
      name, value, default = line.text.scan(Script::MATCH)[0]
      raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath variable declarations.",
        :line => @line + 1) unless line.children.empty?
      raise SyntaxError.new("Invalid variable: \"#{line.text}\".",
        :line => @line) unless name && value

      expr = parse_script(value, :offset => line.offset + line.text.index(value))

      Tree::VariableNode.new(name, expr, default)
    end

    def parse_comment(line)
      if line.text[1] == CSS_COMMENT_CHAR || line.text[1] == SASS_COMMENT_CHAR
        silent = line.text[1] == SASS_COMMENT_CHAR
        if loud = line.text[2] == SASS_LOUD_COMMENT_CHAR
          value = self.class.parse_interp(line.text, line.index, line.offset, :filename => @filename)
          value[0].slice!(2) # get rid of the "!"
        else
          value = [line.text]
        end
        value = with_extracted_values(value) do |str|
          str = str.gsub(/^#{line.comment_tab_str}/m, '')[2..-1] # get rid of // or /*
          format_comment_text(str, silent)
        end
        Tree::CommentNode.new(value, silent, loud)
      else
        Tree::RuleNode.new(parse_interp(line))
      end
    end

    def parse_directive(parent, line, root)
      directive, whitespace, value = line.text[1..-1].split(/(\s+)/, 2)
      offset = directive.size + whitespace.size + 1 if whitespace

      # If value begins with url( or ",
      # it's a CSS @import rule and we don't want to touch it.
      if directive == "import"
        parse_import(line, value)
      elsif directive == "mixin"
        parse_mixin_definition(line)
      elsif directive == "include"
        parse_mixin_include(line, root)
      elsif directive == "function"
        parse_function(line, root)
      elsif directive == "for"
        parse_for(line, root, value)
      elsif directive == "each"
        parse_each(line, root, value)
      elsif directive == "else"
        parse_else(parent, line, value)
      elsif directive == "while"
        raise SyntaxError.new("Invalid while directive '@while': expected expression.") unless value
        Tree::WhileNode.new(parse_script(value, :offset => offset))
      elsif directive == "if"
        raise SyntaxError.new("Invalid if directive '@if': expected expression.") unless value
        Tree::IfNode.new(parse_script(value, :offset => offset))
      elsif directive == "debug"
        raise SyntaxError.new("Invalid debug directive '@debug': expected expression.") unless value
        raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath debug directives.",
          :line => @line + 1) unless line.children.empty?
        offset = line.offset + line.text.index(value).to_i
        Tree::DebugNode.new(parse_script(value, :offset => offset))
      elsif directive == "extend"
        raise SyntaxError.new("Invalid extend directive '@extend': expected expression.") unless value
        raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath extend directives.",
          :line => @line + 1) unless line.children.empty?
        offset = line.offset + line.text.index(value).to_i
        Tree::ExtendNode.new(parse_interp(value, offset))
      elsif directive == "warn"
        raise SyntaxError.new("Invalid warn directive '@warn': expected expression.") unless value
        raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath warn directives.",
          :line => @line + 1) unless line.children.empty?
        offset = line.offset + line.text.index(value).to_i
        Tree::WarnNode.new(parse_script(value, :offset => offset))
      elsif directive == "return"
        raise SyntaxError.new("Invalid @return: expected expression.") unless value
        raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath return directives.",
          :line => @line + 1) unless line.children.empty?
        offset = line.offset + line.text.index(value).to_i
        Tree::ReturnNode.new(parse_script(value, :offset => offset))
      elsif directive == "charset"
        name = value && value[/\A(["'])(.*)\1\Z/, 2] #"
        raise SyntaxError.new("Invalid charset directive '@charset': expected string.") unless name
        raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath charset directives.",
          :line => @line + 1) unless line.children.empty?
        Tree::CharsetNode.new(name)
      elsif directive == "media"
        Tree::MediaNode.new(value)
      else
        Tree::DirectiveNode.new(line.text)
      end
    end

    def parse_for(line, root, text)
      var, from_expr, to_name, to_expr = text.scan(/^([^\s]+)\s+from\s+(.+)\s+(to|through)\s+(.+)$/).first

      if var.nil? # scan failed, try to figure out why for error message
        if text !~ /^[^\s]+/
          expected = "variable name"
        elsif text !~ /^[^\s]+\s+from\s+.+/
          expected = "'from <expr>'"
        else
          expected = "'to <expr>' or 'through <expr>'"
        end
        raise SyntaxError.new("Invalid for directive '@for #{text}': expected #{expected}.")
      end
      raise SyntaxError.new("Invalid variable \"#{var}\".") unless var =~ Script::VALIDATE

      var = var[1..-1]
      parsed_from = parse_script(from_expr, :offset => line.offset + line.text.index(from_expr))
      parsed_to = parse_script(to_expr, :offset => line.offset + line.text.index(to_expr))
      Tree::ForNode.new(var, parsed_from, parsed_to, to_name == 'to')
    end

    def parse_each(line, root, text)
      var, list_expr = text.scan(/^([^\s]+)\s+in\s+(.+)$/).first

      if var.nil? # scan failed, try to figure out why for error message
        if text !~ /^[^\s]+/
          expected = "variable name"
        elsif text !~ /^[^\s]+\s+from\s+.+/
          expected = "'in <expr>'"
        end
        raise SyntaxError.new("Invalid for directive '@each #{text}': expected #{expected}.")
      end
      raise SyntaxError.new("Invalid variable \"#{var}\".") unless var =~ Script::VALIDATE

      var = var[1..-1]
      parsed_list = parse_script(list_expr, :offset => line.offset + line.text.index(list_expr))
      Tree::EachNode.new(var, parsed_list)
    end

    def parse_else(parent, line, text)
      previous = parent.children.last
      raise SyntaxError.new("@else must come after @if.") unless previous.is_a?(Tree::IfNode)

      if text
        if text !~ /^if\s+(.+)/
          raise SyntaxError.new("Invalid else directive '@else #{text}': expected 'if <expr>'.")
        end
        expr = parse_script($1, :offset => line.offset + line.text.index($1))
      end

      node = Tree::IfNode.new(expr)
      append_children(node, line.children, false)
      previous.add_else node
      nil
    end

    def parse_import(line, value)
      raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath import directives.",
        :line => @line + 1) unless line.children.empty?

      scanner = StringScanner.new(value)
      values = []

      loop do
        unless node = parse_import_arg(scanner)
          raise SyntaxError.new("Invalid @import: expected file to import, was #{scanner.rest.inspect}",
            :line => @line)
        end
        values << node
        break unless scanner.scan(/,\s*/)
      end

      if scanner.scan(/;/)
        raise SyntaxError.new("Invalid @import: expected end of line, was \";\".",
          :line => @line)
      end

      return values
    end

    def parse_import_arg(scanner)
      return if scanner.eos?
      unless (str = scanner.scan(Sass::SCSS::RX::STRING)) ||
          (uri = scanner.scan(Sass::SCSS::RX::URI))
        return Tree::ImportNode.new(scanner.scan(/[^,;]+/))
      end

      val = scanner[1] || scanner[2]
      scanner.scan(/\s*/)
      if media = scanner.scan(/[^,;].*/)
        Tree::DirectiveNode.new("@import #{str || uri} #{media}")
      elsif uri
        Tree::DirectiveNode.new("@import #{uri}")
      elsif val =~ /^http:\/\//
        Tree::DirectiveNode.new("@import url(#{val})")
      else
        Tree::ImportNode.new(val)
      end
    end

    MIXIN_DEF_RE = /^(?:=|@mixin)\s*(#{Sass::SCSS::RX::IDENT})(.*)$/
    def parse_mixin_definition(line)
      name, arg_string = line.text.scan(MIXIN_DEF_RE).first
      raise SyntaxError.new("Invalid mixin \"#{line.text[1..-1]}\".") if name.nil?

      offset = line.offset + line.text.size - arg_string.size
      args = Script::Parser.new(arg_string.strip, @line, offset, @options).
        parse_mixin_definition_arglist
      Tree::MixinDefNode.new(name, args)
    end

    MIXIN_INCLUDE_RE = /^(?:\+|@include)\s*(#{Sass::SCSS::RX::IDENT})(.*)$/
    def parse_mixin_include(line, root)
      name, arg_string = line.text.scan(MIXIN_INCLUDE_RE).first
      raise SyntaxError.new("Invalid mixin include \"#{line.text}\".") if name.nil?

      offset = line.offset + line.text.size - arg_string.size
      args, keywords = Script::Parser.new(arg_string.strip, @line, offset, @options).
        parse_mixin_include_arglist
      raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath mixin directives.",
        :line => @line + 1) unless line.children.empty?
      Tree::MixinNode.new(name, args, keywords)
    end

    FUNCTION_RE = /^@function\s*(#{Sass::SCSS::RX::IDENT})(.*)$/
    def parse_function(line, root)
      name, arg_string = line.text.scan(FUNCTION_RE).first
      raise SyntaxError.new("Invalid function definition \"#{line.text}\".") if name.nil?

      offset = line.offset + line.text.size - arg_string.size
      args = Script::Parser.new(arg_string.strip, @line, offset, @options).
        parse_function_definition_arglist
      Tree::FunctionNode.new(name, args)
    end

    def parse_script(script, options = {})
      line = options[:line] || @line
      offset = options[:offset] || 0
      Script.parse(script, line, offset, @options)
    end

    def format_comment_text(text, silent)
      content = text.split("\n")

      if content.first && content.first.strip.empty?
        removed_first = true
        content.shift
      end

      return silent ? "//" : "/* */" if content.empty?
      content.last.gsub!(%r{ ?\*/ *$}, '')
      content.map! {|l| l.gsub!(/^\*( ?)/, '\1') || (l.empty? ? "" : " ") + l}
      content.first.gsub!(/^ /, '') unless removed_first
      if silent
        "//" + content.join("\n//")
      else
        # The #gsub fixes the case of a trailing */
        "/*" + content.join("\n *").gsub(/ \*\Z/, '') + " */"
      end
    end

    def parse_interp(text, offset = 0)
      self.class.parse_interp(text, @line, offset, :filename => @filename)
    end

    # It's important that this have strings (at least)
    # at the beginning, the end, and between each Script::Node.
    #
    # @private
    def self.parse_interp(text, line, offset, options)
      res = []
      rest = Sass::Shared.handle_interpolation text do |scan|
        escapes = scan[2].size
        res << scan.matched[0...-2 - escapes]
        if escapes % 2 == 1
          res << "\\" * (escapes - 1) << '#{'
        else
          res << "\\" * [0, escapes - 1].max
          res << Script::Parser.new(
            scan, line, offset + scan.pos - scan.matched_size, options).
            parse_interpolated
        end
      end
      res << rest
    end
  end
end
