# A visitor for converting a Sass tree into CSS.
class Sass::Tree::Visitors::ToCss < Sass::Tree::Visitors::Base
  def render(node, build_source_mapping = false)
    @build_source_mapping = build_source_mapping
    visit(node)
  end

  def source_mapping
    @source_mapping if @build_source_mapping
  end

  protected

  class VisitorContext
    attr_reader :source_mapping

    attr_reader :parent

    def get(name)
      return @values[name] if @values.has_key?(name)
      return nil if !@parent
      @parent.get(name)
    end

    def []=(name, value)
      @values[name] = value
    end

    def initialize(parent = nil)
      @parent = parent
      @values = {}
    end
  end

  def push_context(opts = {})
    @context = VisitorContext.new(@context)
    opts.each {|k, v|  @context[k] = v}
    @context
  end

  def push_lstrip_context
    push_context(:lstrip => true)
  end

  def pop_context
    child_context = @context
    @context = @context.parent
    child_context
  end

  def initialize()
    @tabs = 0
    @line = 1
    @column = 1
    @result = String.new
    @context = VisitorContext.new
    @start_positions = []
    @source_mapping = Sass::Tree::SourceMapping.new
  end

  def visit(node)
    super
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  def with_tabs(tabs)
    old_tabs, @tabs = @tabs, tabs
    yield
  ensure
    @tabs = old_tabs
  end

  def mark_node_start
    return if !@build_source_mapping
    @start_positions.push(Sass::Tree::SourcePosition.new(@line - 1, @column - 1))
  end

  def mark_node_end(node, range_reader_name = :source_range, filename_reader_name = nil)
    return if !@build_source_mapping
    start_pos = @start_positions.pop
    return if node.invisible? || !node.respond_to?(range_reader_name) || !node.send(range_reader_name)
    original_range = node.send(range_reader_name)
    generated_range = Sass::Tree::SourceRange.new(start_pos, Sass::Tree::SourcePosition.new(@line - 1, @column - 1))
    @source_mapping.add(original_range, generated_range, filename_reader_name && node.respond_to?(filename_reader_name) && node.send(filename_reader_name) ? node.send(filename_reader_name) : node.options[:filename])
  end

  NEWLINE = "\n"

  def strip_trailing_char!(char = nil)
    return if char && (last_char = @result[@result.length - 1, 1]) != char
    if last_char == NEWLINE
      @line -= 1
      @column = 1
    else
      @column -= 1
    end

    @result.slice!(@result.length - 1)
  end

  def output(s, opts = {})
    return if !s
    if @context.get(:lstrip)
      s.gsub!(/\A\s+/, "")
      pop_context
    end

    newline_count = s.count(NEWLINE)
    if newline_count > 0
      @line += newline_count
      @column = Sass::Util::char_size(s.slice(s.rindex(NEWLINE), s.length))
    else
      @column += Sass::Util::char_size(s)
    end
    @result << s
    if opts[:style] && opts[:style] != :compressed
      insert_newline
    end
  end

  def rstrip_output
    suffix = @result.scan(/[\s\n]+\Z/)[0]
    return if !suffix
    suffix.reverse.each_char do |c|
      if c == NEWLINE
        # Encountered "\n"
        @column = 1
        @line -= 1
      else
        @column -= 1
      end
    end
    @result.rstrip!

    # Compute @column after [possibly] stripping newlines
    last_line_column = 1
    for idx in (1..@result.length)
      char = @result[@result.length - idx, 1]
      if !char.match(/\s/)
        break
      end
      last_line_column += 1
    end
    @column = last_line_column
  end

  def insert_newline
    output(NEWLINE)
  end

  def shift_source_mappings(prefix)
    line_delta = prefix.count("\n")
    first_line_col_delta = line_delta > 0 ? Sass::Util.char_size(prefix.slice(prefix.rindex(?\n), prefix.length)) + 1 : 0
    @source_mapping.shift_to_ranges(line_delta, first_line_col_delta)
  end

  def visit_root(node)
    node.children.each_index do |index|
      next if node.children[index].invisible?
      visit(node.children[index])
      insert_newline unless node.style == :compressed
    end
    rstrip_output
    return "" if @result.empty?
    insert_newline
    unless Sass::Util.ruby1_8? || @result.ascii_only?
      if node.children.first.is_a?(Sass::Tree::CharsetNode)
        begin
          encoding = node.children.first.name
          # Default to big-endian encoding, because we have to decide somehow
          encoding << 'BE' if encoding =~ /\Autf-(16|32)\Z/i
          @result = @result.encode(Encoding.find(encoding))
        rescue EncodingError
        end
      end

      charsetPrefix = "@charset \"#{@result.encoding.name}\";#{
        node.style == :compressed ? '' : "\n"
      }"
      shift_source_mappings(charsetPrefix)
      @result = charsetPrefix.encode(@result.encoding) + @result
    end
    @result
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  def visit_charset(node)
    mark_node_start
    output("@charset \"#{node.name}\";")
    mark_node_end(node)
  end

  def visit_comment(node)
    return if node.invisible?
    mark_node_start
    spaces = ('  ' * [@tabs - node.resolved_value[/^ */].size, 0].max)

    content = node.resolved_value.gsub(/^/, spaces)
    content.gsub!(%r{^(\s*)//(.*)$}) {|md| "#{$1}/*#{$2} */"} if node.type == :silent
    content.gsub!(/\n +(\* *(?!\/))?/, ' ') if (node.style == :compact || node.style == :compressed) && node.type != :loud
    output(content)
    mark_node_end(node)
  end

  def lstrip_output
    context = push_lstrip_context
    yield
    pop_context if @context == context
  end

  def visit_directive(node)
    was_in_directive = @in_directive
    tab_str = '  ' * @tabs
    if !node.has_children || node.children.empty?
      output(tab_str)
      mark_node_start
      output(node.resolved_value)
      mark_node_end(node)
      return output(";") unless node.has_children
      return output(" {}") if node.children.empty?
      return
    end

    @in_directive = @in_directive || !node.is_a?(Sass::Tree::MediaNode)
    output("#{tab_str}") if node.style != :compressed
    mark_node_start
    output(node.resolved_value)
    mark_node_end(node)
    output(node.style == :compressed ? "{" : " {")
    output(node.style == :compact ? ' ' : "\n") if node.style != :compressed

    was_prop = false
    first = true
    node.children.each do |child|
      next if child.invisible?
      if node.style == :compact
        if child.is_a?(Sass::Tree::PropNode)
          with_tabs(first || was_prop ? 0 : @tabs + 1) do
            visit(child)
            output(' ')
          end
        else
          if was_prop
            strip_trailing_char!
            output(NEWLINE)
          end

          if first
            lstrip_output {with_tabs(@tabs + 1) {visit(child)}}
          else
            with_tabs(@tabs + 1) {visit(child)}
          end

          rstrip_output
          insert_newline
        end
        was_prop = child.is_a?(Sass::Tree::PropNode)
        first = false
      elsif node.style == :compressed
        output(was_prop ? ";" : "")
        with_tabs(0) {visit(child)}
        was_prop = child.is_a?(Sass::Tree::PropNode)
      else
        with_tabs(@tabs + 1) {visit(child)}
        insert_newline
      end
    end
    rstrip_output
    appended = if node.style == :compressed
                 "}"
               else
                 (node.style == :expanded ? "\n" : " ") + "}\n"
               end
    output(appended)
  ensure
    @in_directive = was_in_directive
  end

  def visit_media(node)
    with_tabs(@tabs + node.tabs) {visit_directive(node)}
    strip_trailing_char!(NEWLINE) unless node.style == :compressed || node.group_end
  end

  def visit_supports(node)
    visit_media(node)
  end

  def visit_cssimport(node)
    visit_directive(node)
  end

  def visit_prop(node)
    return if node.resolved_value.empty?
    tab_str = '  ' * (@tabs + node.tabs)
    output(tab_str)
    mark_node_start
    output(node.resolved_name)
    mark_node_end(node)
    if node.style == :compressed
      output(":");
      mark_node_start
      output(node.resolved_value)
      mark_node_end(node, :value_source_range, node.respond_to?(:value_original_filename) ? :value_original_filename : :filename)
    else
      output(": ")
      mark_node_start
      output(node.resolved_value)
      mark_node_end(node, :value_source_range, node.respond_to?(:value_original_filename) ? :value_original_filename : :filename)
      output(";")
    end
  end

  def visit_rule(node)
    with_tabs(@tabs + node.tabs) do
      rule_separator = node.style == :compressed ? ',' : ', '
      line_separator =
        case node.style
          when :nested, :expanded; "\n"
          when :compressed; ""
          else; " "
        end
      rule_indent = '  ' * @tabs
      per_rule_indent, total_indent = [:nested, :expanded].include?(node.style) ? [rule_indent, ''] : ['', rule_indent]

      joined_rules = node.resolved_rules.members.map do |seq|
        next if seq.has_placeholder?
        rule_part = seq.to_a.join
        if node.style == :compressed
          rule_part.gsub!(/([^,])\s*\n\s*/m, '\1 ')
          rule_part.gsub!(/\s*([,+>])\s*/m, '\1')
          rule_part.strip!
        end
        rule_part
      end.compact.join(rule_separator)

      joined_rules.sub!(/\A\s*/, per_rule_indent)
      joined_rules.gsub!(/\s*\n\s*/, "#{line_separator}#{per_rule_indent}")
      total_rule = total_indent << joined_rules

      old_spaces = '  ' * @tabs
      spaces = '  ' * (@tabs + 1)
      if node.style != :compressed
        if node.options[:debug_info] && !@in_directive
          visit(debug_info_rule(node.debug_info, node.options))
          insert_newline
        elsif node.options[:trace_selectors]
          output("#{old_spaces}/* ")
          output(node.stack_trace.join("\n   #{old_spaces}"))
          output(" */\n")
        elsif node.options[:line_comments]
          output("#{old_spaces}/* line #{node.line}")

          if node.filename
            relative_filename = if node.options[:css_filename]
              begin
                Pathname.new(node.filename).relative_path_from(
                  Pathname.new(File.dirname(node.options[:css_filename]))).to_s
              rescue ArgumentError
                nil
              end
            end
            relative_filename ||= node.filename
            output(", #{relative_filename}")
          end

          output(" */\n")
        end
      end

      end_props = ''
      tabs = 0
      trailer = ''
      if node.style == :compact
        separator = ' '
        end_props = " "
        trailer = "\n" if node.group_end
      elsif node.style == :compressed
        separator = ';'
      else
        tabs = @tabs + 1
        separator = "\n"
        trailer = "\n" if node.group_end
        end_props = (node.style == :expanded ? "\n" + old_spaces : ' ')
      end
      output(total_rule)
      case node.style
      when :compact
        output(" { ")
      when :compressed
        output("{")
      else
        output(" {\n")
      end

      with_tabs(tabs) do
        node.children.each_index do |i|
          output(separator) if i > 0
          visit(node.children[i])
        end
      end

      output(end_props) if end_props
      output("}" + trailer)
    end
  end

  private

  def debug_info_rule(debug_info, options)
    node = Sass::Tree::DirectiveNode.resolved("@media -sass-debug-info")
    Sass::Util.hash_to_a(debug_info.map {|k, v| [k.to_s, v.to_s]}).each do |k, v|
      rule = Sass::Tree::RuleNode.new([""])
      rule.resolved_rules = Sass::Selector::CommaSequence.new(
        [Sass::Selector::Sequence.new(
            [Sass::Selector::SimpleSequence.new(
                [Sass::Selector::Element.new(k.to_s.gsub(/[^\w-]/, "\\\\\\0"), nil)],
                false)
            ])
        ])
      prop = Sass::Tree::PropNode.new([""], Sass::Script::String.new(''), :new)
      prop.resolved_name = "font-family"
      prop.resolved_value = Sass::SCSS::RX.escape_ident(v.to_s)
      rule << prop
      node << rule
    end
    node.options = options.merge(:debug_info => false, :line_comments => false, :style => :compressed)
    node
  end
end
