# A visitor for converting a Sass tree into CSS.
class Sass::Tree::Visitors::ToCss < Sass::Tree::Visitors::Base
  protected

  def initialize
    @tabs = 0
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

  def visit_root(node)
    result = String.new
    node.children.each do |child|
      next if child.invisible?
      child_str = visit(child)
      result << child_str + (node.style == :compressed ? '' : "\n")
    end
    result.rstrip!
    return "" if result.empty?
    result << "\n"
    unless Sass::Util.ruby1_8? || result.ascii_only?
      if node.children.first.is_a?(Sass::Tree::CharsetNode)
        begin
          encoding = node.children.first.name
          # Default to big-endian encoding, because we have to decide somehow
          encoding << 'BE' if encoding =~ /\Autf-(16|32)\Z/i
          result = result.encode(Encoding.find(encoding))
        rescue EncodingError
        end
      end

      result = "@charset \"#{result.encoding.name}\";#{
        node.style == :compressed ? '' : "\n"
      }".encode(result.encoding) + result
    end
    result
  rescue Sass::SyntaxError => e
    e.sass_template ||= node.template
    raise e
  end

  def visit_charset(node)
    "@charset \"#{node.name}\";"
  end 

  def visit_comment(node)
    return if node.invisible?
    spaces = ('  ' * [@tabs - node.resolved_value[/^ */].size, 0].max)

    content = node.resolved_value.gsub(/^/, spaces).gsub(%r{^(\s*)//(.*)$}) do |md|
      "#{$1}/*#{$2} */"
    end
    content.gsub!(/\n +(\* *(?!\/))?/, ' ') if (node.style == :compact || node.style == :compressed) && !node.loud
    content
  end

  def visit_directive(node)
    was_in_directive = @in_directive
    tab_str = '  ' * @tabs
    return tab_str + node.value + ";" unless node.has_children
    return tab_str + node.value + " {}" if node.children.empty?
    @in_directive = @in_directive || !node.is_a?(Sass::Tree::MediaNode)
    result = if node.style == :compressed
               "#{node.value}{"
             else
               "#{tab_str}#{node.value} {" + (node.style == :compact ? ' ' : "\n")
             end
    was_prop = false
    first = true
    node.children.each do |child|
      next if child.invisible?
      if node.style == :compact
        if child.is_a?(Sass::Tree::PropNode)
          with_tabs(first || was_prop ? 0 : @tabs + 1) {result << visit(child) << ' '}
        else
          result[-1] = "\n" if was_prop
          rendered = with_tabs(@tabs + 1) {visit(child).dup}
          rendered = rendered.lstrip if first
          result << rendered.rstrip + "\n"
        end
        was_prop = child.is_a?(Sass::Tree::PropNode)
        first = false
      elsif node.style == :compressed
        result << (was_prop ? ";" : "") << with_tabs(0) {visit(child)}
        was_prop = child.is_a?(Sass::Tree::PropNode)
      else
        result << with_tabs(@tabs + 1) {visit(child)} + "\n"
      end
    end
    result.rstrip + if node.style == :compressed
                      "}"
                    else
                      (node.style == :expanded ? "\n" : " ") + "}\n"
                    end
  ensure
    @in_directive = was_in_directive
  end

  def visit_media(node)
    str = with_tabs(@tabs + node.tabs) {visit_directive(node)}
    str.gsub!(/\n\Z/, '') unless node.style == :compressed || node.group_end
    str
  end

  def visit_prop(node)
    tab_str = '  ' * (@tabs + node.tabs)
    if node.style == :compressed
      "#{tab_str}#{node.resolved_name}:#{node.resolved_value}"
    else
      "#{tab_str}#{node.resolved_name}: #{node.resolved_value};"
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
        rule_part = seq.to_a.join
        if node.style == :compressed
          rule_part.gsub!(/([^,])\s*\n\s*/m, '\1 ')
          rule_part.gsub!(/\s*([,+>])\s*/m, '\1')
          rule_part.strip!
        end
        rule_part
      end.join(rule_separator)

      joined_rules.sub!(/\A\s*/, per_rule_indent)
      joined_rules.gsub!(/\s*\n\s*/, "#{line_separator}#{per_rule_indent}")
      total_rule = total_indent << joined_rules

      to_return = ''
      old_spaces = '  ' * @tabs
      spaces = '  ' * (@tabs + 1)
      if node.style != :compressed
        if node.options[:debug_info] && !@in_directive
          to_return << visit(debug_info_rule(node.debug_info, node.options)) << "\n"
        elsif node.options[:trace_selectors]
          to_return << "#{old_spaces}/* "
          to_return << node.stack_trace.join("\n   #{old_spaces}")
          to_return << " */\n"
        elsif node.options[:line_comments]
          to_return << "#{old_spaces}/* line #{node.line}"

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
            to_return << ", #{relative_filename}"
          end

          to_return << " */\n"
        end
      end

      if node.style == :compact
        properties = with_tabs(0) {node.children.map {|a| visit(a)}.join(' ')}
        to_return << "#{total_rule} { #{properties} }#{"\n" if node.group_end}"
      elsif node.style == :compressed
        properties = with_tabs(0) {node.children.map {|a| visit(a)}.join(';')}
        to_return << "#{total_rule}{#{properties}}"
      else
        properties = with_tabs(@tabs + 1) {node.children.map {|a| visit(a)}.join("\n")}
        end_props = (node.style == :expanded ? "\n" + old_spaces : ' ')
        to_return << "#{total_rule} {\n#{properties}#{end_props}}#{"\n" if node.group_end}"
      end

      to_return
    end
  end

  private

  def debug_info_rule(debug_info, options)
    node = Sass::Tree::DirectiveNode.new("@media -sass-debug-info")
    Sass::Util.hash_to_a(debug_info.map {|k, v| [k.to_s, v.to_s]}).each do |k, v|
      rule = Sass::Tree::RuleNode.new([""])
      rule.resolved_rules = Sass::Selector::CommaSequence.new(
        [Sass::Selector::Sequence.new(
            [Sass::Selector::SimpleSequence.new(
                [Sass::Selector::Element.new(k.to_s.gsub(/[^\w-]/, "\\\\\\0"), nil)])
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
