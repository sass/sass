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
    spaces = ('  ' * [@tabs - node.value[/^ */].size, 0].max)

    content = node.value.gsub(/^/, spaces).gsub(%r{^(\s*)//(.*)$}) do |md|
      "#{$1}/*#{$2} */"
    end
    if content =~ /[^\\]\#\{.*\}/
      Sass::Util.sass_warn <<MESSAGE
WARNING:
On line #{node.line}#{" of '#{node.filename}'" if node.filename}
Comments will evaluate the contents of interpolations (\#{ ... }) in Sass 3.2.
Please escape the interpolation by adding a backslash before the hash sign.
MESSAGE
    elsif content =~ /\\\#\{.*\}/
      content.gsub!(/\\(\#\{.*\})/, '\1')
    end
    content.gsub!(/\n +(\* *(?!\/))?/, ' ') if (node.style == :compact || node.style == :compressed) && !node.loud
    content
  end

  def visit_directive(node)
    return node.value + ";" unless node.has_children
    return node.value + " {}" if node.children.empty?
    result = if node.style == :compressed
               "#{node.value}{"
             else
               "#{'  ' * @tabs}#{node.value} {" + (node.style == :compact ? ' ' : "\n")
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
  end

  def visit_media(node)
    str = with_tabs(@tabs + node.tabs) {visit_directive(node)}
    str.gsub!(/\n\Z/, '') unless node.style == :compressed || node.group_end
    str
  end

  def visit_prop(node)
    tab_str = '  ' * (@tabs + node.tabs)
    prop = node.resolved_name
    value = node.resolved_value
    if node.flip
      # Flip property names, e.g. border-right, left, padding-left, etc.
      if prop.include?("left")
        prop["left"] = "right"
      elsif prop.include?("right")
        prop["right"] = "left"
      # Flip properties with left/right values, e.g. float, text-align, etc.
      elsif ["float", "clear", "text-align", "ruby-align", "text-align-last", "caption-side", "tab-side"].include?(prop)
        if value.include?("left")
          value["left"] = "right"
        elsif value.include?("right")
          value["right"] = "left"
        end
      # Flip margin/padding/border-width properties with 4 values (including
      # optional !important, etc.)
      elsif ["margin", "padding", "border-width"].include?(prop)
        split = value.split()
        if split.length >= 4
          split[1], split[3] = split[3], split[1]
          value = split.join(" ")
        end
      # Flip any border-width value within the shorthand border property.
      elsif prop == "border"
        value.sub!(/(((\s*[0-9]+(px|em|ch|cm|ex|gd|in|mm|pc|pt|rem|vh|vw|vm)[^\S]*)|(\s*(medium|thick|thin)[^\S]*)){4})/) do |m|
          match = $1
          prefix = match.scan(/^\s*/)[0]
          suffix = match.scan(/\s*$/)[0]
          split = match.split()
          split[1], split[3] = split[3], split[1]
          prefix + split.join(" ") + suffix
        end
      # Flip variants of the border-radius property.
      elsif ["border-radius", "-moz-border-radius", "-webkit-border-radius"].include?(prop)
        split = value.split('/')
        split.map! do |v|
          elems = v.split()
          if elems[-1] == "!important"
            important = true
            elems.pop()
          else
            important = false
          end
          el_length = elems.length
          if el_length == 4
            elems = [elems[1], elems[0], elems[3], elems[2]]
          elsif el_length == 2
            elems = [elems[1], elems[0]]
          elsif el_length == 3
            elems = [elems[1], elems[0], elems[1], elems[2]]
          elsif not el_length == 1
            raise "Unsupported number of value elements in #{prop}: #{value}"
          end
          if important
            elems.join(" ") + " !important"
          else
            elems.join(" ")
          end
        end
        value = split.join(" / ")
      # Flip plain left/right and percentage-based background-position,
      # perspective-origin and transform-origin values.
      elsif prop == "background-position" ||
          prop.include?("transform-origin") ||
          prop.include?("perspective-origin")
        if value.include?("left")
          value["left"] = "right"
        elsif value.include?("right")
          value["right"] = "left"
        elsif value.include?("%")
          split = value.split()
          xpercent = split[0]
          if xpercent.include?("%")
            split[0] = String(100 - Integer(xpercent[0...-1]))
            value = split.join(" ")
          end
        end
      # Flip any background-position value within the shorthand background
      # property.
      elsif prop == "background"
        if /(^|[^\S])left([^\S]|$)/.match(value)
          value.gsub!(/(^|[^\S])left([^\S]|$)/, '\\1right\\2')
        else
          if /(^|[^\S])([0-9]{1,2})%([^\S]|$)/.match(value)
            value.sub!(/(^|[^\S])([0-9]{1,2})%([^\S]|$)/) do |m|
              "#{$1}#{100 - Integer($2)}%#{$3}"
            end
          else
            value.gsub!(/(^|[^\S])right([^\S]|$)/, '\\1left\\2')
          end
        end
      # Flip cursor property values like sw-resize.
      elsif prop == "cursor"
        if /([ns]?)e-resize/.match(value)
          value.gsub!(/([ns]?)e-resize/, '\\1w-resize')
        else
          value.gsub!(/([ns]?)w-resize/, '\\1e-resize')
        end
      end
      # We don't handle the following properties where x-pos is defined in any
      # units other than percentage, e.g. 6px.
      #
      #   background-position: x-pos y-pos
      #   perspective-origin: x-pos y-pos
      #   transform-origin: x-pos y-pos
      #
      # We also don't currently support the box/text-shadow variants.
    end
    if node.style == :compressed
      "#{tab_str}#{prop}:#{value}"
    else
      "#{tab_str}#{prop}: #{value};"
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
        rule_part.gsub!(/\s*([^,])\s*\n\s*/m, '\1 ') if node.style == :compressed
        rule_part
      end.join(rule_separator)

      joined_rules.sub!(/\A\s*/, per_rule_indent)
      joined_rules.gsub!(/\s*\n\s*/, "#{line_separator}#{per_rule_indent}")
      total_rule = total_indent << joined_rules

      to_return = ''
      old_spaces = '  ' * @tabs
      spaces = '  ' * (@tabs + 1)
      if node.style != :compressed
        if node.options[:debug_info]
          to_return << visit(debug_info_rule(node.debug_info, node.options)) << "\n"
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
    debug_info.map {|k, v| [k.to_s, v.to_s]}.sort.each do |k, v|
      rule = Sass::Tree::RuleNode.new([""])
      rule.resolved_rules = Sass::Selector::CommaSequence.new(
        [Sass::Selector::Sequence.new(
            [Sass::Selector::SimpleSequence.new(
                [Sass::Selector::Element.new(k.to_s.gsub(/[^\w-]/, "\\\\\\0"), nil)])
            ])
        ])
      prop = Sass::Tree::PropNode.new([""], "", :new)
      prop.resolved_name = "font-family"
      prop.resolved_value = Sass::SCSS::RX.escape_ident(v.to_s)
      rule << prop
      node << rule
    end
    node.options = options.merge(:debug_info => false, :line_comments => false, :style => :compressed)
    node
  end
end
