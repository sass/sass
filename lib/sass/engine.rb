require 'sass/tree/node'
require 'sass/tree/value_node'
require 'sass/tree/rule_node'
require 'sass/tree/comment_node'
require 'sass/tree/attr_node'
require 'sass/tree/directive_node'
require 'sass/constant'
require 'sass/error'

module Sass
  # This is the class where all the parsing and processing of the Sass
  # template is done. It can be directly used by the user by creating a
  # new instance and calling <tt>render</tt> to render the template. For example:
  #
  #   template = File.load('stylesheets/sassy.sass')
  #   sass_engine = Sass::Engine.new(template)
  #   output = sass_engine.render
  #   puts output
  class Engine
    # The character that begins a CSS attribute.
    ATTRIBUTE_CHAR  = ?:

    # The character that designates that
    # an attribute should be assigned to the result of constant arithmetic.
    SCRIPT_CHAR     = ?=

    # The character that designates the beginning of a comment,
    # either Sass or CSS.
    COMMENT_CHAR = ?/

    # The character that follows the general COMMENT_CHAR and designates a Sass comment,
    # which is not output as a CSS comment.
    SASS_COMMENT_CHAR = ?/

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
    # attributes of the form <tt>:name attr</tt>.
    ATTRIBUTE = /^:([^\s=:]+)\s*(=?)(?:\s+|$)(.*)/

    # The regex that matches attributes of the form <tt>name: attr</tt>.
    ATTRIBUTE_ALTERNATE_MATCHER = /^[^\s:]+\s*[=:](\s|$)/

    # The regex that matches and extracts data from
    # attributes of the form <tt>name: attr</tt>.
    ATTRIBUTE_ALTERNATE = /^([^\s=:]+)(\s*=|:)(?:\s+|$)(.*)/

    # Creates a new instace of Sass::Engine that will compile the given
    # template string when <tt>render</tt> is called.
    # See README.rdoc for available options.
    #
    #--
    #
    # TODO: Add current options to REFRENCE. Remember :filename!
    #
    # When adding options, remember to add information about them
    # to README.rdoc!
    #++
    #
    def initialize(template, options={})
      @options = {
        :style => :nested,
        :load_paths => ['.']
      }.merge! options
      @template = template.split(/\r\n|\r|\n/)
      @lines = []
      @constants = {"important" => "!important"}
      @mixins = {}
    end

    # Processes the template and returns the result as a string.
    def render
      begin
        render_to_tree.to_s
      rescue SyntaxError => err
        unless err.sass_filename
          err.add_backtrace_entry(@options[:filename])
        end
        raise err
      end
    end

    alias_method :to_css, :render

    protected

    def constants
      @constants
    end

    def mixins
      @mixins
    end

    def render_to_tree
      split_lines

      root = Tree::Node.new(@options[:style])
      index = 0
      while @lines[index]
        old_index = index
        child, index = build_tree(index)

        if child.is_a? Tree::Node
          child.line = old_index + 1
          root << child
        elsif child.is_a? Array
          child.each do |c|
            root << c
          end
        end
      end
      @lines.clear

      root
    end

    private

    # Readies each line in the template for parsing,
    # and computes the tabulation of the line.
    def split_lines
      @line = 0
      old_tabs = nil
      @template.each_with_index do |line, index|
        @line += 1

        tabs = count_tabs(line)

        if line[0] == COMMENT_CHAR && line[1] == SASS_COMMENT_CHAR && tabs == 0
          tabs = old_tabs
        end

        if tabs # if line isn't blank
          raise SyntaxError.new("Indenting at the beginning of the document is illegal.", @line) if old_tabs.nil? && tabs > 0

          if old_tabs && tabs - old_tabs > 1
            raise SyntaxError.new("#{tabs * 2} spaces were used for indentation. Sass must be indented using two spaces.", @line)
          end
          @lines << [line.strip, tabs]

          old_tabs = tabs
        else
          @lines << ['//', old_tabs || 0]
        end
      end

      @line = nil
    end

    # Counts the tabulation of a line.
    def count_tabs(line)
      return nil if line.strip.empty?
      return nil unless spaces = line.index(/[^ ]/)

      if spaces % 2 == 1
          raise SyntaxError.new(<<END.strip, @line)
#{spaces} space#{spaces == 1 ? ' was' : 's were'} used for indentation. Sass must be indented using two spaces.
END
      elsif line[spaces] == ?\t
        raise SyntaxError.new(<<END.strip, @line)
A tab character was used for indentation. Sass must be indented using two spaces.
Are you sure you have soft tabs enabled in your editor?
END
      end
      spaces / 2
    end

    def build_tree(index)
      line, tabs = @lines[index]
      index += 1
      @line = index
      node = parse_line(line)

      has_children = has_children?(index, tabs)

      # Node is a symbol if it's non-outputting, like a constant assignment
      unless node.is_a? Tree::Node
        if has_children
          if node == :constant
            raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath constants.", @line + 1)
          elsif node.is_a? Array
            # arrays can either be full of import statements
            # or attributes from mixin includes
            # in either case they shouldn't have children.
            # Need to peek into the array in order to give meaningful errors
            directive_type = (node.first.is_a?(Tree::DirectiveNode) ? "import" : "mixin")
            raise SyntaxError.new("Illegal nesting: Nothing may be nested beneath #{directive_type} directives.", @line + 1)
          end
        end

        index = @line if node == :mixin
        return node, index
      end

      node.line = @line

      if node.is_a? Tree::CommentNode
        while has_children
          line, index = raw_next_line(index)
          node << line

          has_children = has_children?(index, tabs)
        end

        return node, index
      end

      # Resolve multiline rules
      if node.is_a?(Tree::RuleNode)
        if node.continued?
          child, index = build_tree(index) if @lines[old_index = index]
          if @lines[old_index].nil? || has_children?(old_index, tabs) || !child.is_a?(Tree::RuleNode)
            raise SyntaxError.new("Rules can't end in commas.", @line)
          end

          node.add_rules child
        end
        node.children = child.children if child
      end

      while has_children
        child, index = build_tree(index)

        validate_and_append_child(node, child)

        has_children = has_children?(index, tabs)
      end

      return node, index
    end

    def validate_and_append_child(parent, child)
      case child
      when :constant
        raise SyntaxError.new("Constants may only be declared at the root of a document.", @line)
      when :mixin
        raise SyntaxError.new("Mixins may only be defined at the root of a document.", @line)
      when Array
        child.each do |c|
          if c.is_a?(Tree::DirectiveNode)
            raise SyntaxError.new("Import directives may only be used at the root of a document.", @line)
          end
          parent << c
        end
      when Tree::Node
        parent << child
      end
    end

    def has_children?(index, tabs)
      next_line = ['//', 0]
      while !next_line.nil? && next_line[0] == '//' && next_line[1] = 0
        next_line = @lines[index]
        index += 1
      end
      next_line && next_line[1] > tabs
    end

    def raw_next_line(index)
      [@lines[index][0], index + 1]
    end

    def parse_line(line)
      case line[0]
      when ATTRIBUTE_CHAR
        if line[1] != ATTRIBUTE_CHAR
          parse_attribute(line, ATTRIBUTE)
        else
          # Support CSS3-style pseudo-elements,
          # which begin with ::
          Tree::RuleNode.new(line, @options[:style])
        end
      when Constant::CONSTANT_CHAR
        parse_constant(line)
      when COMMENT_CHAR
        parse_comment(line)
      when DIRECTIVE_CHAR
        parse_directive(line)
      when ESCAPE_CHAR
        Tree::RuleNode.new(line[1..-1], @options[:style])
      when MIXIN_DEFINITION_CHAR
        parse_mixin_definition(line)
      when MIXIN_INCLUDE_CHAR
        if line[1].nil? || line[1] == ?\s
          Tree::RuleNode.new(line, @options[:style])
        else
          parse_mixin_include(line)
        end
      else
        if line =~ ATTRIBUTE_ALTERNATE_MATCHER
          parse_attribute(line, ATTRIBUTE_ALTERNATE)
        else
          Tree::RuleNode.new(line, @options[:style])
        end
      end
    end

    def parse_attribute(line, attribute_regx)
      if @options[:attribute_syntax] == :normal &&
          attribute_regx == ATTRIBUTE_ALTERNATE
        raise SyntaxError.new("Illegal attribute syntax: can't use alternate syntax when :attribute_syntax => :normal is set.")
      elsif @options[:attribute_syntax] == :alternate &&
          attribute_regx == ATTRIBUTE
        raise SyntaxError.new("Illegal attribute syntax: can't use normal syntax when :attribute_syntax => :alternate is set.")
      end

      name, eq, value = line.scan(attribute_regx)[0]

      if name.nil? || value.nil?
        raise SyntaxError.new("Invalid attribute: \"#{line}\".", @line)
      end

      if eq.strip[0] == SCRIPT_CHAR
        value = Sass::Constant.parse(value, @constants, @line).to_s
      end

      Tree::AttrNode.new(name, value, @options[:style])
    end

    def parse_constant(line)
      name, op, value = line.scan(Sass::Constant::MATCH)[0]
      unless name && value
        raise SyntaxError.new("Invalid constant: \"#{line}\".", @line)
      end

      constant = Sass::Constant.parse(value, @constants, @line)
      if op == '||='
        @constants[name] ||= constant
      else
        @constants[name] = constant
      end

      :constant
    end

    def parse_comment(line)
      if line[1] == SASS_COMMENT_CHAR
        :comment
      elsif line[1] == CSS_COMMENT_CHAR
        Tree::CommentNode.new(line, @options[:style])
      else
        Tree::RuleNode.new(line, @options[:style])
      end
    end

    def parse_directive(line)
      directive, value = line[1..-1].split(/\s+/, 2)

      # If value begins with url( or ",
      # it's a CSS @import rule and we don't want to touch it.
      if directive == "import" && value !~ /^(url\(|")/
        import(value)
      else
        Tree::DirectiveNode.new(line, @options[:style])
      end
    end

    def parse_mixin_definition(line)
      mixin_name = line[1..-1].strip
      @mixins[mixin_name] =  []
      index = @line
      line, tabs = @lines[index]
      while !line.nil? && tabs > 0
        child, index = build_tree(index)
        validate_and_append_child(@mixins[mixin_name], child)
        line, tabs = @lines[index]
      end
      :mixin
    end

    def parse_mixin_include(line)
      mixin_name = line[1..-1]
      unless @mixins.has_key?(mixin_name)
        raise SyntaxError.new("Undefined mixin '#{mixin_name}'.", @line)
      end
      @mixins[mixin_name]
    end

    def import(files)
      nodes = []

      files.split(/,\s*/).each do |filename|
        engine = nil

        begin
          filename = self.class.find_file_to_import(filename, @options[:load_paths])
        rescue Exception => e
          raise SyntaxError.new(e.message, @line)
        end

        if filename =~ /\.css$/
          nodes << Tree::DirectiveNode.new("@import url(#{filename})", @options[:style])
        else
          File.open(filename) do |file|
            new_options = @options.dup
            new_options[:filename] = filename
            engine = Sass::Engine.new(file.read, @options)
          end

          engine.constants.merge! @constants
          engine.mixins.merge! @mixins

          begin
            root = engine.render_to_tree
          rescue Sass::SyntaxError => err
            err.add_backtrace_entry(filename)
            raise err
          end
          root.children.each do |child|
            child.filename = filename
            nodes << child
          end
          @constants = engine.constants
          @mixins = engine.mixins
        end
      end

      nodes
    end

    def self.find_file_to_import(filename, load_paths)
      was_sass = false
      original_filename = filename

      if filename[-5..-1] == ".sass"
        filename = filename[0...-5]
        was_sass = true
      elsif filename[-4..-1] == ".css"
        return filename
      end

      new_filename = find_full_path("#{filename}.sass", load_paths)

      return new_filename if new_filename
      return filename + '.css' unless was_sass
      raise SyntaxError.new("File to import not found or unreadable: #{original_filename}.", @line)
    end

    def self.find_full_path(filename, load_paths)
      load_paths.each do |path|
        ["_#{filename}", filename].each do |name|
          full_path = File.join(path, name)
          if File.readable?(full_path)
            return full_path
          end
        end
      end
      nil
    end
  end
end
