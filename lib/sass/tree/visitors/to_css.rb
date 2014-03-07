# A visitor for converting a Sass tree into CSS.
class Sass::Tree::Visitors::ToCss < Sass::Tree::Visitors::Base
  # The source mapping for the generated CSS file. This is only set if
  # `build_source_mapping` is passed to the constructor and \{Sass::Engine#render} has been
  # run.
  attr_reader :source_mapping

  # @param build_source_mapping [Boolean] Whether to build a
  #   \{Sass::Source::Map} while creating the CSS output. The mapping will
  #   be available from \{#source\_mapping} after the visitor has completed.
  def initialize(build_source_mapping = false)
    @tabs = 0
    @line = 1
    @offset = 1
    @result = ""
    @source_mapping = Sass::Source::Map.new if build_source_mapping
  end

  # Runs the visitor on `node`.
  #
  # @param node [Sass::Tree::Node] The root node of the tree to convert to CSS>
  # @return [String] The CSS output.
  def visit(node)
    super
  rescue Sass::SyntaxError => e
    e.modify_backtrace(:filename => node.filename, :line => node.line)
    raise e
  end

  protected

  def with_tabs(tabs)
    old_tabs, @tabs = @tabs, tabs
    yield
  ensure
    @tabs = old_tabs
  end

  # Associate all output produced in a block with a given node. Used for source
  # mapping.
  def for_node(node, attr_prefix = nil)
    return yield unless @source_mapping
    start_pos = Sass::Source::Position.new(@line, @offset)
    yield

    range_attr = attr_prefix ? :"#{attr_prefix}_source_range" : :source_range
    return if node.invisible? || !node.send(range_attr)
    source_range = node.send(range_attr)
    target_end_pos = Sass::Source::Position.new(@line, @offset)
    target_range = Sass::Source::Range.new(start_pos, target_end_pos, nil)
    @source_mapping.add(source_range, target_range)
  end

  # Move the output cursor back `chars` characters.
  def erase!(chars)
    return if chars == 0
    str = @result.slice!(-chars..-1)
    newlines = str.count("\n")
    if newlines > 0
      @line -= newlines
      @offset = @result[@result.rindex("\n") || 0..-1].size
    else
      @offset -= chars
    end
  end

  # Avoid allocating lots of new strings for `#output`. This is important
  # because `#output` is called all the time.
  NEWLINE = "\n"

  # Add `s` to the output string and update the line and offset information
  # accordingly.
  def output(s)
    if @lstrip
      s = s.gsub(/\A\s+/, "")
      @lstrip = false
    end

    newlines = s.count(NEWLINE)
    if newlines > 0
      @line += newlines
      @offset = s[s.rindex(NEWLINE)..-1].size
    else
      @offset += s.size
    end

    @result << s
  end

  # Strip all trailing whitespace from the output string.
  def rstrip!
    erase! @result.length - 1 - (@result.rindex(/[^\s]/) || -1)
  end

  # lstrip the first output in the given block.
  def lstrip
    old_lstrip = @lstrip
    @lstrip = true
    yield
  ensure
    @lstrip = @lstrip && old_lstrip
  end

  # Prepend `prefix` to the output string.
  def prepend!(prefix)
    @result.insert 0, prefix
    return unless @source_mapping

    line_delta = prefix.count("\n")
    offset_delta = prefix.gsub(/.*\n/, '').size
    @source_mapping.shift_output_offsets(offset_delta)
    @source_mapping.shift_output_lines(line_delta)
  end

  def visit_root(node)
    node.children.each do |child|
      next if child.invisible?
      visit(child)
      unless node.style == :compressed
        output "\n"
        if child.is_a?(Sass::Tree::DirectiveNode) && child.has_children && !child.bubbles?
          output "\n"
        end
      end
    end
    rstrip!
    return "" if @result.empty?

    output "\n"
    return @result if Sass::Util.ruby1_8? || @result.ascii_only?

    if node.children.first.is_a?(Sass::Tree::CharsetNode)
      begin
        encoding = node.children.first.name
        # Default to big-endian encoding, because we have to decide somehow
        encoding << 'BE' if encoding =~ /\Autf-(16|32)\Z/i
        @result = @result.encode(Encoding.find(encoding))
      rescue EncodingError
      end
    end

    prepend! "@charset \"#{@result.encoding.name}\";#{
      node.style == :compressed ? '' : "\n"
    }".encode(@result.encoding)
    @result
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  def visit_charset(node)
    for_node(node) {output("@charset \"#{node.name}\";")}
  end

  def visit_comment(node)
    return if node.invisible?
    spaces = ('  ' * [@tabs - node.resolved_value[/^ */].size, 0].max)

    content = node.resolved_value.gsub(/^/, spaces)
    if node.type == :silent
      content.gsub!(%r{^(\s*)//(.*)$}) {|md| "#{$1}/*#{$2} */"}
    end
    if (node.style == :compact || node.style == :compressed) && node.type != :loud
      content.gsub!(/\n +(\* *(?!\/))?/, ' ')
    end
    for_node(node) {output(content)}
  end

  # @comment
  #   rubocop:disable MethodLength
  def visit_directive(node)
    was_in_directive = @in_directive
    tab_str = '  ' * @tabs
    if !node.has_children || node.children.empty?
      output(tab_str)
      for_node(node) {output(node.resolved_value)}
      output(!node.has_children ? ";" : " {}")
      return
    end

    @in_directive = @in_directive || !node.is_a?(Sass::Tree::MediaNode)
    output(tab_str) if node.style != :compressed
    for_node(node) {output(node.resolved_value)}
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
            erase! 1
            output "\n"
          end

          if first
            lstrip {with_tabs(@tabs + 1) {visit(child)}}
          else
            with_tabs(@tabs + 1) {visit(child)}
          end

          rstrip!
          output "\n"
        end
        was_prop = child.is_a?(Sass::Tree::PropNode)
        first = false
      elsif node.style == :compressed
        output(was_prop ? ";" : "")
        with_tabs(0) {visit(child)}
        was_prop = child.is_a?(Sass::Tree::PropNode)
      else
        with_tabs(@tabs + 1) {visit(child)}
        output "\n"
      end
    end
    rstrip!
    if node.style == :expanded
      output("\n#{tab_str}")
    elsif node.style != :compressed
      output(" ")
    end
    output("}")
  ensure
    @in_directive = was_in_directive
  end
  # @comment
  #   rubocop:enable MethodLength

  def visit_media(node)
    with_tabs(@tabs + node.tabs) {visit_directive(node)}
    output("\n") if node.group_end
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
    for_node(node, :name) {output(node.resolved_name)}
    if node.style == :compressed
      output(":")
      for_node(node, :value) {output(node.resolved_value)}
    else
      output(": ")
      for_node(node, :value) {output(node.resolved_value)}
      output(";")
    end
  end

  # @comment
  #   rubocop:disable MethodLength
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
      per_rule_indent, total_indent = if [:nested, :expanded].include?(node.style)
                                        [rule_indent, '']
                                      else
                                        ['', rule_indent]
                                      end

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

      joined_rules.lstrip!
      joined_rules.gsub!(/\s*\n\s*/, "#{line_separator}#{per_rule_indent}")

      old_spaces = '  ' * @tabs
      if node.style != :compressed
        if node.options[:debug_info] && !@in_directive
          visit(debug_info_rule(node.debug_info, node.options))
          output "\n"
        elsif node.options[:trace_selectors]
          output("#{old_spaces}/* ")
          output(node.stack_trace.gsub("\n", "\n   #{old_spaces}"))
          output(" */\n")
        elsif node.options[:line_comments]
          output("#{old_spaces}/* line #{node.line}")

          if node.filename
            relative_filename =
              if node.options[:css_filename]
                begin
                  Sass::Util.pathname(node.filename).relative_path_from(
                    Sass::Util.pathname(File.dirname(node.options[:css_filename]))).to_s
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

      end_props, trailer, tabs  = '', '', 0
      if node.style == :compact
        separator, end_props, bracket = ' ', ' ', ' { '
        trailer = "\n" if node.group_end
      elsif node.style == :compressed
        separator, bracket = ';', '{'
      else
        tabs = @tabs + 1
        separator, bracket = "\n", " {\n"
        trailer = "\n" if node.group_end
        end_props = (node.style == :expanded ? "\n" + old_spaces : ' ')
      end
      output(total_indent + per_rule_indent)
      for_node(node, :selector) {output(joined_rules)}
      output(bracket)

      with_tabs(tabs) do
        node.children.each_with_index do |child, i|
          output(separator) if i > 0
          visit(child)
        end
      end

      output(end_props)
      output("}" + trailer)
    end
  end
  # @comment
  #   rubocop:enable MethodLength

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
      prop = Sass::Tree::PropNode.new([""], Sass::Script::Value::String.new(''), :new)
      prop.resolved_name = "font-family"
      prop.resolved_value = Sass::SCSS::RX.escape_ident(v.to_s)
      rule << prop
      node << rule
    end
    node.options = options.merge(:debug_info => false,
                                 :line_comments => false,
                                 :style => :compressed)
    node
  end
end
